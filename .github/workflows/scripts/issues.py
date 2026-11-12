#!/usr/bin/env python3
import hashlib
import html
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path


STAGES = ("structure", "drafting", "language", "integrity")
MARKER = re.compile(r"<!-- constitution-finding:v1:([0-9a-f]{24}) -->")
UPDATE_MARKER = "<!-- constitution-finding-update:v1:{} -->"


def finding_key(code, section, message):
    phrase, separator, _ = message.partition(": ")
    identity = phrase if code.startswith("LAW") and separator else "" if code == "NUM000" else message
    return code, section.casefold(), identity.casefold()


def collect(directory, document):
    lines = document.splitlines()
    headings = []
    section = "Preamble"
    for text in lines:
        if text.startswith("## "):
            section = text[3:].strip()
        headings.append(section)

    logs = [directory / f"{stage}.log" for stage in STAGES]
    if not any(path.exists() for path in logs):
        return {}
    findings = {}
    for stage, path in zip(STAGES, logs):
        status_path = directory / f"{stage}.status"
        if not path.exists() or not status_path.exists():
            raise ValueError(f"incomplete {stage} check; refusing to open partial findings")
        observed_errors = 0
        for record in path.read_text(encoding="utf-8").splitlines():
            if record.startswith("STAT\t"):
                continue
            severity, code, location, message = record.split("\t", 3)
            if severity not in {"ERROR", "WARN", "WARNING", "SUGGESTION"}:
                raise ValueError(f"unexpected {stage} severity: {severity}")
            observed_errors += severity == "ERROR"
            line = int(location.removeprefix("line "))
            if not 1 <= line <= len(lines) or not code or not message:
                raise ValueError(f"invalid {stage} finding: {record}")
            section = headings[line - 1]
            findings.setdefault(finding_key(code, section, message), [code, section, message, set()])[3].add(line)
        status = dict(row.split("=", 1) for row in status_path.read_text(encoding="utf-8").splitlines() if "=" in row)
        if int(status["errors"]) != observed_errors:
            raise ValueError(f"incomplete {stage} check; reported errors do not match findings")
    return findings


def issue(code, section, message, locations, repository, commit):
    phrase, separator, action = message.partition(": ")
    point = action if code.startswith("LAW") and separator else message
    if code == "NUM000":
        point = "Confirm the intended clause numbering so internal references can be checked"
        title = "Review canonical clause numbering"
    elif code == "DRAFT001":
        point = "Resolve or remove the editor comment before publication"
        title = f"Resolve an editor comment — {section}"
    elif code == "Constitution.Spelling":
        word = re.search(r"'([^']+)'", message)
        if word:
            point = f"Check whether “{word.group(1)}” is a typo and correct it if needed"
            title = f"Check spelling of “{word.group(1)}” — {section}"
        else:
            title = f"Check a possible typo — {section}"
    elif code.startswith("REF"):
        target = re.search(r"reference ([0-9(][0-9A-Za-z().-]*)", message)
        if target:
            point = f"Confirm the intended target of the reference to {target.group(1)}, then correct the cross-reference"
            title = f"Clarify reference to {target.group(1)} — {section}"
        else:
            point = "Identify the intended provision and correct the cross-reference"
            title = f"Clarify an internal reference — {section}"
    elif code.startswith("ROLE"):
        office = message.partition(" is used")[0] if " is used" in message else "the office name"
        title = f"Define or clarify {office} — {section}"
    else:
        title = f"{point.split(';', 1)[0].rstrip('.')} — {section}"
    title = re.sub(r"\s+", " ", title).replace("@", " at ")[:120].rstrip()
    title = title[0].upper() + title[1:]
    digest = hashlib.sha256("\0".join(finding_key(code, section, message)).encode()).hexdigest()[:24]
    safe = lambda value: html.escape(value, quote=False).replace("@", "&#64;")
    body = f"**Point for review:** {safe(point.rstrip('.'))}.\n\n"
    if code.startswith("LAW") and separator:
        body += f"**Wording:** {safe(phrase)}\n\n"
    if code.startswith("REF"):
        body += f"**Check detail:** {safe(message)}\n\n"
    body += f"**Section:** {safe(section)}\n\n**Location{'' if len(locations) == 1 else 's'}:**\n"
    for line in sorted(locations)[:8]:
        body += f"- [Constitution.md, line {line}](https://github.com/{repository}/blob/{commit}/Constitution.md#L{line})\n"
    if len(locations) > 8:
        body += f"- {len(locations) - 8} further occurrence(s) in this section\n"
    return title, body + f"\n<!-- constitution-finding:v1:{digest} -->\n", digest


def gh(endpoint, payload=None, paginate=False):
    command = ["gh", "api", endpoint]
    if paginate:
        command += ["--paginate", "--slurp"]
    if payload is not None:
        command += ["-X", "POST", "--input", "-"]
    result = subprocess.run(
        command, input=json.dumps(payload) if payload is not None else None,
        text=True, capture_output=True, check=False,
    )
    if result.returncode:
        raise RuntimeError(result.stderr.strip() or f"GitHub API failed: {endpoint}")
    return json.loads(result.stdout)


def normalized(value):
    return re.sub(r"[\W_]+", " ", value.casefold()).strip()


def similar(existing, title, body):
    previous = existing.get("body") or ""
    section = re.search(r"^\*\*Section:\*\* (.+)$", body, re.MULTILINE)
    previous_section = re.search(r"^\*\*Section:\*\* (.+)$", previous, re.MULTILINE)
    point = re.search(r"^\*\*Point for review:\*\* (.+)$", body, re.MULTILINE)
    previous_point = re.search(r"^\*\*Point for review:\*\* (.+)$", previous, re.MULTILINE)
    if section and previous_section:
        if normalized(section[1]) != normalized(previous_section[1]):
            return False
        if point and previous_point:
            return normalized(point[1]) == normalized(previous_point[1])
    return not title.startswith("Check a possible typo") and normalized(existing.get("title") or "") == normalized(title)


def update_comment(body):
    evidence = re.sub(r"/blob/[0-9a-f]{40}/", "/blob/COMMIT/", body)
    digest = hashlib.sha256(evidence.encode()).hexdigest()[:24]
    detail = body.split("\n<!-- constitution-finding:v1:", 1)[0].strip()
    return f"The constitution check found this point in the current text.\n\n{detail}\n\n{UPDATE_MARKER.format(digest)}\n", digest


def open_issues(proposals, repository, api=gh, pause=time.sleep):
    pages = api(f"repos/{repository}/issues?state=all&per_page=100", paginate=True)
    existing = [item for page in pages for item in page if "pull_request" not in item]
    created = updated = unchanged = 0
    for title, body, digest in proposals:
        match = None
        for state in ("open", "closed"):
            candidates = [item for item in existing if item.get("state") == state]
            match = next((item for item in candidates if digest in MARKER.findall(item.get("body") or "")), None)
            if match is None:
                match = next((item for item in candidates if similar(item, title, body)), None)
            if match is not None:
                break
        if match is not None:
            comment, update_digest = update_comment(body)
            comments = api(f"repos/{repository}/issues/{match['number']}/comments?per_page=100", paginate=True)
            if any(UPDATE_MARKER.format(update_digest) in (item.get("body") or "") for page in comments for item in page):
                unchanged += 1
                continue
            if created or updated:
                pause(1)
            api(f"repos/{repository}/issues/{match['number']}/comments", {"body": comment})
            print(f"Updated #{match['number']}: {title}")
            updated += 1
            continue
        if created or updated:
            pause(1)
        result = api(f"repos/{repository}/issues", {"title": title, "body": body})
        print(f"Opened #{result['number']}: {title}")
        existing.append({"number": result["number"], "title": title, "body": body, "state": "open"})
        created += 1
    print(f"{created} issue(s) opened; {updated} updated; {unchanged} already up to date")
    return created


def selfcheck():
    from tempfile import TemporaryDirectory
    from unittest.mock import patch

    with TemporaryDirectory() as temporary:
        directory = Path(temporary)
        for stage in STAGES:
            (directory / f"{stage}.status").write_text("result=passed\nerrors=0\n")
            (directory / f"{stage}.log").write_text("")
        (directory / "drafting.log").write_text(
            "WARN\tLAW002\t3\tand/or: State whether both or either is intended\n"
            "WARN\tLAW002\tline 4\tAnd/or: State whether both or either is intended\n"
        )
        found = collect(directory, "# Constitution\n## Membership\nOne and/or two.\nThree and/or four.\n")
        assert len(found) == 1 and next(iter(found.values()))[3] == {3, 4}
        code, section, message, locations = next(iter(found.values()))
        title, body, digest = issue(code, section, message, locations, "owner/repo", "a" * 40)
        assert "Membership" in title and "line 3" in body and "line 4" in body
        assert MARKER.search(body).group(1) == digest
        (directory / "integrity.status").unlink()
        try:
            collect(directory, "# Constitution\n## Membership\nText\n")
        except ValueError:
            pass
        else:
            raise AssertionError("partial checks must not create issues")
    with patch("subprocess.run", return_value=subprocess.CompletedProcess([], 0, "[[\"ok\"]]", "")) as call:
        assert gh("repos/owner/repo/issues?state=all&per_page=100", paginate=True) == [["ok"]]
        assert call.call_args.args[0][-2:] == ["--paginate", "--slurp"]
    with patch("subprocess.run", return_value=subprocess.CompletedProcess([], 0, '{"number":1}', "")) as call:
        assert gh("repos/owner/repo/issues", {"title": "Review", "body": "Text"})["number"] == 1
        assert call.call_args.args[0][-3:] == ["POST", "--input", "-"]
    posted = []
    comments = []

    def fake_api(endpoint, payload=None, paginate=False):
        if payload is None:
            assert paginate
            return [comments] if "/comments?" in endpoint else [posted]
        if endpoint.endswith("/comments"):
            comments.append(payload)
            return {"id": len(comments)}
        posted.append({**payload, "number": len(posted), "state": "open"})
        return posted[-1]

    proposal = (title, body, digest)
    assert open_issues([proposal], "owner/repo", fake_api, lambda _: None) == 1
    assert open_issues([proposal], "owner/repo", fake_api, lambda _: None) == 0
    assert open_issues([proposal], "owner/repo", fake_api, lambda _: None) == 0
    assert len(posted) == len(comments) == 1 and posted[0]["title"] == title
    assert similar(posted[0], title, body)
    assert not similar({**posted[0], "body": posted[0]["body"].replace("State whether both or either is intended", "Choose one alternative")}, title, body)
    changed_commit = body.replace("/blob/" + "a" * 40 + "/", "/blob/" + "b" * 40 + "/")
    assert update_comment(body)[1] == update_comment(changed_commit)[1]
    posted[:] = [{"number": 7, "state": "open", "title": "Review this provision", "body": body.split("\n<!-- constitution-finding:v1:", 1)[0]}]
    comments.clear()
    assert open_issues([proposal], "owner/repo", fake_api, lambda _: None) == 0
    assert len(posted) == len(comments) == 1
    first_typo = issue("Constitution.Spelling", "Membership", "Spelling: check 'thr'.", {3}, "owner/repo", "a" * 40)
    second_typo = issue("Constitution.Spelling", "Membership", "Spelling: check 'wher'.", {3}, "owner/repo", "a" * 40)
    assert not similar({"title": first_typo[0], "body": first_typo[1]}, second_typo[0], second_typo[1])
    print("Issue self-check: passed")


def main():
    if sys.argv[1:] == ["--selfcheck"]:
        selfcheck()
        return
    if sys.argv[1:] not in ([], ["--preview"]):
        raise SystemExit("usage: issues.py [--preview|--selfcheck]")
    directory = Path(os.environ.get("CHECK_DIR") or Path(os.environ.get("RUNNER_TEMP") or os.environ.get("TMPDIR") or "/tmp") / f"uqrl-constitution-checks-{os.getuid()}")
    found = collect(directory, Path("Constitution.md").read_text(encoding="utf-8"))
    if not found:
        print("No completed findings to open as issues")
        return
    repository = os.environ.get("GITHUB_REPOSITORY", "")
    commit = os.environ.get("GITHUB_SHA", "")
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repository) or not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise ValueError("valid GITHUB_REPOSITORY and GITHUB_SHA are required")
    proposals = [issue(*found[key][:3], found[key][3], repository, commit) for key in sorted(found)]
    if sys.argv[1:] == ["--preview"]:
        for title, _, _ in proposals:
            print(title)
        return
    if os.environ.get("GITHUB_ACTIONS") != "true" or os.environ.get("GITHUB_REF") != "refs/heads/main" or not os.environ.get("GH_TOKEN"):
        raise ValueError("issue creation requires an authenticated main-branch Actions run")
    open_issues(proposals, repository)


if __name__ == "__main__":
    main()
