#!/usr/bin/env bash
set -euo pipefail

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
output_dir=${1:-dist}
[[ "$#" -le 1 && -n "$output_dir" ]] || { echo 'usage: render [output-directory]' >&2; exit 2; }
command -v typst >/dev/null || { echo 'render: typst is required' >&2; exit 2; }
[[ -s Constitution.md ]] || { echo 'render: Constitution.md is missing or empty' >&2; exit 2; }
[[ $(wc -c < Constitution.md) -le 1048576 ]] || { echo 'render: Constitution.md exceeds the 1 MiB document limit' >&2; exit 2; }
year=${PUBLICATION_YEAR:-$(date -u +%Y)}
[[ "$year" =~ ^[1-9][0-9]{3}$ ]] || { echo 'render: PUBLICATION_YEAR must be a four-digit year' >&2; exit 2; }
mkdir -p "$output_dir"
output_abs=$(cd "$output_dir" && pwd -P)
case "$output_abs" in "$root"/*) ;; *) echo 'render: output directory must be inside the repository' >&2; exit 2 ;; esac
pdf="$output_abs/constitution-$year.pdf"

typst compile --root "$root" --ignore-system-fonts --creation-timestamp "${SOURCE_DATE_EPOCH:-$(date -u +%s)}" tools/config/constitution.typ "$pdf"
[[ -s "$pdf" && $(head -c 5 "$pdf") == '%PDF-' ]] || { echo "render: invalid PDF: $pdf" >&2; exit 2; }
if command -v sha256sum >/dev/null; then digest=$(sha256sum "$pdf" | awk '{print $1}'); else digest=$(shasum -a 256 "$pdf" | awk '{print $1}'); fi
printf 'PDF: %s\nSHA-256: %s\n' "$pdf" "$digest"
