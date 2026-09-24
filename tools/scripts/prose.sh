#!/usr/bin/env bash
set -euo pipefail

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
vale_version=3.15.1
language_filter='.Name in ["Constitution.Spelling", "Constitution.Language", "Vale.Repetition"]'
temporary=

cleanup() {
  if [[ -n "$temporary" && -d "$temporary" ]]; then
    rm -rf -- "$temporary"
  fi
}
trap cleanup EXIT

fail() {
  printf 'prose: %s\n' "$*" >&2
  exit 2
}

require() {
  command -v "$1" >/dev/null 2>&1 || fail "required command not found: $1"
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

platform_archive() {
  case "$(uname -s):$(uname -m)" in
    Linux:x86_64) printf 'vale_%s_Linux_64-bit.tar.gz\n' "$vale_version" ;;
    Linux:aarch64 | Linux:arm64) printf 'vale_%s_Linux_arm64.tar.gz\n' "$vale_version" ;;
    Darwin:x86_64) printf 'vale_%s_macOS_64-bit.tar.gz\n' "$vale_version" ;;
    Darwin:arm64) printf 'vale_%s_macOS_arm64.tar.gz\n' "$vale_version" ;;
    *) fail "Vale $vale_version has no configured archive for $(uname -s) $(uname -m)" ;;
  esac
}

prepare() {
  if [[ -n "${VALE_BIN:-}" ]]; then
    [[ -x "$VALE_BIN" ]] || fail "Vale executable not found: $VALE_BIN"
    "$VALE_BIN" --version | grep -Fq 'vale version ' || fail "$VALE_BIN is not Vale"
    printf '%s\n' "$VALE_BIN"
    return
  fi
  require curl
  require tar

  local cache archive base checksums expected actual bin
  if [[ -n "${RUNNER_TEMP:-}" ]]; then
    cache="$RUNNER_TEMP/vale/$vale_version"
  else
    cache="${XDG_CACHE_HOME:-$HOME/.cache}/uqrl-constitution/vale/$vale_version"
  fi
  bin="$cache/vale"

  if [[ ! -x "$bin" ]]; then
    archive=$(platform_archive)
    base="https://github.com/vale-cli/vale/releases/download/v$vale_version"
    temporary=$(mktemp -d)
    curl --fail --silent --show-error --location --output "$temporary/$archive" "$base/$archive" \
      || fail "could not download the official Vale archive"
    curl --fail --silent --show-error --location --output "$temporary/checksums.txt" "$base/vale_${vale_version}_checksums.txt" \
      || fail "could not download the official Vale checksum manifest"
    checksums="$temporary/checksums.txt"
    expected=$(awk -v archive="$archive" '$2 == archive {print $1}' "$checksums")
    [[ "$expected" =~ ^[0-9a-f]{64}$ ]] || fail "official Vale checksum for $archive was not found"
    actual=$(sha256 "$temporary/$archive")
    [[ "$actual" == "$expected" ]] || fail "Vale archive checksum mismatch"
    tar -xzf "$temporary/$archive" -C "$temporary"
    [[ -x "$temporary/vale" ]] || fail "Vale archive did not contain the executable"
    mkdir -p "$cache"
    install -m 0755 "$temporary/vale" "$bin"
  fi

  "$bin" --version | grep -Fq "vale version $vale_version" || fail "$bin is not Vale $vale_version"
  printf 'Vale %s: %s\n' "$vale_version" "$bin" >&2
  printf '%s\n' "$bin"
}

check_dir() {
  printf '%s\n' "${CHECK_DIR:-${TMPDIR:-/tmp}/uqrl-constitution-checks-${UID}}"
}

write_status() {
  local stage=$1 result=$2 errors=$3 warnings=$4 repeated=$5 directory
  directory=$(check_dir)
  mkdir -p "$directory"
  {
    printf 'result=%s\n' "$result"
    printf 'errors=%s\n' "$errors"
    printf 'warnings=%s\n' "$warnings"
    printf 'repeated=%s\n' "$repeated"
  } > "$directory/$stage.status"
}

representative_findings() {
  jq -r '
    [to_entries[] as $file | $file.value[] |
      "- \(.Check) at line \(.Line): \(.Message | gsub("[\\r\\n]"; " "))"
    ][:3][]
  ' "$1"
}

remaining_findings() {
  local total
  total=$(jq '[.[][]] | length' "$1")
  [[ "$total" -le 3 ]] || printf -- '- And %s more; see logs.\n' "$((total - 3))"
}

plural() {
  [[ "$1" == 1 ]] || printf 's'
}

write_fragment() {
  local stage=$1 errors=$2 warnings=$3 repeated=$4 json=$5 directory
  directory=$(check_dir)
  mkdir -p "$directory"
  {
    printf -- '- %s spelling or objective typo error%s.\n' "$errors" "$(plural "$errors")"
    printf -- '- %s warning%s; %s repeated-word finding%s.\n' \
      "$warnings" "$(plural "$warnings")" \
      "$repeated" "$(plural "$repeated")"
    representative_findings "$json"
    remaining_findings "$json"
    [[ "$errors" != 0 || "$warnings" != 0 ]] || printf -- '- Spelling and UQRL vocabulary passed.\n'
  } > "$directory/$stage.md"
}

run_stage() {
  local stage=language document=$1 vale json runtime rc errors warnings repeated result
  [[ -s "$document" ]] || fail "document not found or empty: $document"
  require jq
  vale=${VALE_BIN:-$(prepare)}
  [[ -x "$vale" ]] || fail "Vale executable not found: $vale"
  temporary=$(mktemp -d)
  json="$temporary/vale.json"
  runtime="$temporary/runtime.log"

  set +e
  "$vale" --config="$root/tools/config/.vale.ini" --no-global --no-exit --no-wrap --output=JSON --filter="$language_filter" "$document" > "$json" 2> "$runtime"
  rc=$?
  set -e
  if [[ "$rc" -ne 0 ]]; then
    cat "$runtime" >&2
    write_status "$stage" failed 1 0 0
    printf -- '- Vale runtime failed; see the job log.\n' > "$(check_dir)/$stage.md"
    return 2
  fi
  jq -e 'type == "object"' "$json" >/dev/null || fail "Vale returned invalid JSON"

  errors=$(jq '[.[][] | select((.Severity | ascii_downcase) == "error")] | length' "$json")
  warnings=$(jq '[.[][] | select((.Severity | ascii_downcase) != "error")] | length' "$json")
  repeated=$(jq '[.[][] | select(.Check == "Vale.Repetition")] | length' "$json")
  result=passed
  [[ "$errors" == 0 ]] || result=failed
  write_status "$stage" "$result" "$errors" "$warnings" "$repeated"
  write_fragment "$stage" "$errors" "$warnings" "$repeated" "$json"
  jq -r 'to_entries[] as $file | $file.value[] | "\(.Severity | ascii_upcase)\t\(.Check)\tline \(.Line)\t\(.Message)"' "$json"
  [[ "$errors" == 0 ]]
}

selfcheck() {
  require jq
  local vale passed failed accepted spelling locale repeated correct results
  vale=${VALE_BIN:-$(prepare)}
  temporary=$(mktemp -d)
  accepted="$temporary/accepted.md"
  spelling="$temporary/spelling.md"
  locale="$temporary/locale.md"
  repeated="$temporary/repeated.md"
  correct="$temporary/correct.md"
  printf 'uqrl members organise events.\n' > "$accepted"
  printf 'This zqxword is misspelled.\n' > "$spelling"
  printf 'Members organize events.\n' > "$locale"
  printf 'The members repeat the the rule.\n' > "$repeated"
  printf 'The members organise the annual meeting.\n' > "$correct"

  results="$temporary/results.json"
  "$vale" --config="$root/tools/config/.vale.ini" --no-global --no-exit --no-wrap --output=JSON \
    "$accepted" "$spelling" "$locale" "$repeated" "$correct" > "$results"
  jq -e 'type == "object"' "$results" >/dev/null || fail "Vale returned invalid self-check JSON"

  passed=0
  failed=0
  if ! jq -e --arg file "$accepted" '[.[$file][]? | select(.Check == "Constitution.Spelling" and (.Message | contains("uqrl")))] | length == 0' "$results" >/dev/null; then
    printf 'Prose self-check failed: UQRL vocabulary\n'
    failed=$((failed + 1))
  else passed=$((passed + 1)); fi
  if jq -e --arg file "$spelling" '[.[$file][]? | select(.Check == "Constitution.Spelling")] | length > 0' "$results" >/dev/null; then
    passed=$((passed + 1))
  else printf 'Prose self-check failed: spelling\n'; failed=$((failed + 1)); fi
  if jq -e --arg file "$locale" '[.[$file][]? | select(.Check == "Constitution.Language")] | length > 0' "$results" >/dev/null; then
    passed=$((passed + 1))
  else printf 'Prose self-check failed: Australian spelling\n'; failed=$((failed + 1)); fi
  if jq -e --arg file "$repeated" '[.[$file][]? | select(.Check == "Vale.Repetition")] | length > 0' "$results" >/dev/null; then
    passed=$((passed + 1))
  else printf 'Prose self-check failed: repeated word\n'; failed=$((failed + 1)); fi
  if jq -e --arg file "$correct" '(.[$file] // []) | length == 0' "$results" >/dev/null; then
    passed=$((passed + 1))
  else printf 'Prose self-check failed: correct sentence\n'; failed=$((failed + 1)); fi
  printf 'Prose self-check: %s passed, %s failed\n' "$passed" "$failed"
  [[ "$failed" == 0 ]]
}

usage() {
  printf 'usage: prose prepare | prose language Constitution.md | prose selfcheck\n' >&2
  exit 2
}

case "${1:-}" in
  prepare)
    [[ "$#" == 1 ]] || usage
    prepare
    ;;
  language)
    [[ "$#" == 2 ]] || usage
    run_stage "$2"
    ;;
  selfcheck)
    [[ "$#" == 1 ]] || usage
    selfcheck
    ;;
  *) usage ;;
esac
