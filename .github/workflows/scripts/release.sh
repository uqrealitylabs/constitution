#!/usr/bin/env bash
set -euo pipefail

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../../.." && pwd)
cd "$root"
temporary=$(mktemp -d)
trap 'rm -rf -- "$temporary"' EXIT

fail() {
  printf 'release: %s\n' "$*" >&2
  exit 2
}

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  else
    fail "sha256sum or shasum is required"
  fi
}

semver_less_than() {
  local left=$1 right=$2 left_major left_minor left_patch right_major right_minor right_patch
  IFS=. read -r left_major left_minor left_patch <<< "$left"
  IFS=. read -r right_major right_minor right_patch <<< "$right"
  ((10#$left_major < 10#$right_major)) && return 0
  ((10#$left_major > 10#$right_major)) && return 1
  ((10#$left_minor < 10#$right_minor)) && return 0
  ((10#$left_minor > 10#$right_minor)) && return 1
  ((10#$left_patch < 10#$right_patch))
}

previous_tag() {
  local candidate candidate_version
  while IFS= read -r candidate; do
    [[ "$candidate" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]] || continue
    [[ "$candidate" != "$tag" ]] || continue
    candidate_version=${candidate#v}
    if semver_less_than "$candidate_version" "$version"; then
      printf '%s\n' "$candidate"
      return
    fi
  done < <(git tag --merged HEAD --sort=-version:refname)
}

heading_at_line() {
  local target=$1
  awk -v target="$target" '
    NR <= target && /^## / { heading = substr($0, 4) }
    END { if (heading != "") print heading }
  ' Constitution.md
}

affected_sections() {
  local start=$1 new_start heading count=0
  while IFS= read -r hunk; do
    new_start=
    if [[ "$hunk" =~ ^@@\ -([0-9]+)(,[0-9]+)?\ \+([0-9]+)(,[0-9]+)?\ @@ ]]; then
      new_start=${BASH_REMATCH[3]}
    fi
    [[ -n "$new_start" ]] || continue
    heading=$(heading_at_line "$new_start")
    [[ -n "$heading" ]] || continue
    if ! grep -Fqx -- "$heading" "$temporary/sections" 2>/dev/null; then
      printf '%s\n' "$heading" >> "$temporary/sections"
      count=$((count + 1))
      [[ "$count" -lt 8 ]] || break
    fi
  done < <(git diff --unified=0 "$start" HEAD -- Constitution.md | grep '^@@' || true)
}

write_changes() {
  local range=$1 total limit short subject
  git log --no-merges --format='%H%x09%s' "$range" -- Constitution.md tools/config/constitution.typ tools/scripts/render.sh \
    | awk -F '\t' '!seen[$2]++' > "$temporary/commits"
  total=$(awk 'END {print NR}' "$temporary/commits")
  limit=$total
  [[ "$total" -le 12 ]] || limit=11
  if [[ "$total" -eq 0 ]]; then
    printf -- '- No publication-source commits were found in %s.\n' "\`$range\`"
    return
  fi
  while IFS=$'\t' read -r commit subject; do
    [[ "$limit" -gt 0 ]] || break
    short=${commit:0:7}
    printf -- '- %s (%s)\n' "$subject" "\`$short\`"
    limit=$((limit - 1))
  done < "$temporary/commits"
  if [[ "$total" -gt 12 ]]; then
    printf -- '- %s additional source changes are included in this release.\n' "$((total - 11))"
  fi
}

write_notes() {
  local previous=$1 digest=$2 commit=$3 range
  if [[ -z "$previous" ]]; then
    {
      printf '## Constitution release\n\n'
      printf -- '- Initial PDF release from the repository source.\n\n'
      printf '## Verification\n\n'
      verification "$digest" "$commit"
    } > "$temporary/notes.md"
    printf 'Focused changelog range: repository root..HEAD\n' >&2
    return
  fi

  range="$previous..HEAD"
  affected_sections "$previous"
  {
    printf '## Constitution changes\n\n'
    write_changes "$range"
    printf '\n## Affected sections\n\n'
    if [[ -s "$temporary/sections" ]]; then
      sed 's/^/- /' "$temporary/sections"
    else
      printf -- '- No second-level section headings changed.\n'
    fi
    printf '\n## Verification\n\n'
    verification "$digest" "$commit"
  } > "$temporary/notes.md"
  printf 'Focused changelog range: %s\n' "$range" >&2
}

verification() {
  local digest=$1 commit=$2
  printf -- '- Release: %s\n' "\`$tag\`"
  printf -- '- Source commit: %s\n' "\`${commit:0:12}\`"
  printf -- '- PDF: %s\n' "\`$pdf_name\`"
  printf -- '- SHA-256: %s\n' "\`$digest\`"
}

mode=release
case "$#" in
  0) ;;
  1)
    [[ "$1" == --dry-run ]] || fail "usage: release [--dry-run]"
    mode=dry-run
    ;;
  *) fail "usage: release [--dry-run]" ;;
esac

command -v git >/dev/null 2>&1 || fail "required command not found: git"
semver='(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)'
tag=${GITHUB_REF_NAME:-}
[[ "$tag" =~ ^v$semver$ ]] || fail "release tag is not strict v-prefixed semantic version text: $tag"
version=${tag#v}

year=${PUBLICATION_YEAR:-$(date -u +%Y)}
[[ "$year" =~ ^[1-9][0-9]{3}$ ]] || fail "PUBLICATION_YEAR must be a four-digit year"
pdf_name="constitution-$year.pdf"
pdf="dist/$pdf_name"
[[ -s "$pdf" ]] || fail "expected PDF is missing or empty: $pdf"
[[ $(head -c 5 "$pdf") == "%PDF-" ]] || fail "expected asset is not a PDF: $pdf"
digest=$(sha256 "$pdf")
commit=$(git rev-parse --verify HEAD)
previous=$(previous_tag || true)
write_notes "$previous" "$digest" "$commit"

if [[ "$mode" == dry-run ]]; then
  printf 'Release dry-run for %s\n\n' "$tag"
  cat "$temporary/notes.md"
  exit 0
fi

[[ "${GITHUB_ACTIONS:-}" == true ]] || fail "normal release mode requires GITHUB_ACTIONS=true"
[[ -n "${GH_TOKEN:-}" ]] || fail "normal release mode requires GH_TOKEN"
repository=${GITHUB_REPOSITORY:-}
[[ "$repository" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || fail "GITHUB_REPOSITORY is invalid"
[[ "${GITHUB_REF:-}" == "refs/tags/$tag" ]] || fail "GITHUB_REF is not the validated tag ref"
command -v gh >/dev/null 2>&1 || fail "required command not found: gh"
tag_commit=$(git rev-list -n 1 "$tag^{commit}")
[[ "$tag_commit" == "$commit" ]] || fail "tag $tag does not point at HEAD"
if gh release view "$tag" --repo "$repository" >/dev/null 2>&1; then
  fail "release already exists for $tag"
fi

gh release create "$tag" "$pdf" \
  --repo "$repository" \
  --verify-tag \
  --title "UQ Reality Labs Constitution $tag" \
  --notes-file "$temporary/notes.md"
