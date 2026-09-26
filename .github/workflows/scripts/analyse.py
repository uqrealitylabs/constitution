#!/usr/bin/env python3
import os
from html import escape
from pathlib import Path

import laya
from huggingface_hub import snapshot_download


MODEL = "convaiinnovations/laya"
REVISION = "aa8c91ca088ec597df95a0d1c76b3063cb2ae5e8"
TOKEN_LIMIT = 220
CHUNK_LIMIT = 128
TOPICS = {
    "membership": "Membership and eligibility",
    "committee": "Committee offices and powers",
    "meetings": "Meetings, elections and votes",
    "administration": "Records, funds and administration",
    "rules": "Rule changes and dissolution",
    "other": "Purpose or other matter",
}
QUESTIONS = {
    "topic": {
        "type": "choice",
        "instructions": "What is the main governance subject of this club constitution excerpt?",
        "criteria": {
            "membership": "Joining, eligibility, membership fees or loss of membership",
            "committee": "Committee composition, officers, powers or delegations",
            "meetings": "General meetings, elections, notices, votes or quorum",
            "administration": "Records, documents, accounts, funds or property",
            "rules": "Amendment of rules, winding up or dissolution",
            "other": "Name, aims or another subject",
        },
    }
}


def sections(source):
    title, start, lines = "Preamble", 1, []
    for number, line in enumerate(source.splitlines(keepends=True), 1):
        if line.startswith("## "):
            if lines:
                yield title, start, "".join(lines)
            title, start, lines = line[3:].strip(), number, []
        lines.append(line)
    if lines:
        yield title, start, "".join(lines)


def scan(source, agent):
    rows = []
    for title, line, body in sections(source):
        tokens = agent.tok.encode(body, add_special_tokens=False)
        for offset in range(0, len(tokens), TOKEN_LIMIT):
            if len(rows) >= CHUNK_LIMIT:
                raise ValueError(f"constitution exceeds {CHUNK_LIMIT} Laya excerpts")
            excerpt = agent.tok.decode(tokens[offset : offset + TOKEN_LIMIT], skip_special_tokens=True)
            answer = agent.predict(excerpt, QUESTIONS)["answers"]["topic"]["choice"]
            if answer not in TOPICS:
                raise ValueError(f"Laya returned an unknown topic: {answer!r}")
            rows.append((title, line, offset // TOKEN_LIMIT + 1, TOPICS[answer]))
    if not rows:
        raise ValueError("constitution has no text for Laya to analyse")
    return rows


def report(rows):
    lines = [
        "## Laya document map",
        "",
        "Experimental topic labels only. The legal drafting checks and human review remain authoritative.",
        f"Model: `laya==0.3.11`, checkpoint `{REVISION}`. {len(rows)} excerpts examined.",
        "",
        "| Source | Excerpt | Suggested topic |",
        "|---|---:|---|",
    ]
    for title, line, part, topic in rows:
        safe_title = escape(title).replace("|", "&#124;").replace("[", "&#91;").replace("]", "&#93;")
        lines.append(f"| {safe_title} (line {line}) | {part} | {topic} |")
    return "\n".join(lines) + "\n"


def main():
    path = Path("Constitution.md")
    if not 0 < path.stat().st_size <= 1_048_576:
        raise ValueError("Constitution.md must contain no more than 1 MiB")
    snapshot = snapshot_download(
        repo_id=MODEL,
        revision=REVISION,
        allow_patterns=["model.safetensors", "rl_agent_config.json", "encoder/*", "tokenizer/*"],
    )
    with laya.load(snapshot, device="cpu") as agent:
        output = report(scan(path.read_text(encoding="utf-8"), agent))
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a", encoding="utf-8") as target:
            target.write(output)
    else:
        print(output, end="")


if __name__ == "__main__":
    main()
