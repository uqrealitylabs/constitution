#!/usr/bin/env bash
set -euo pipefail

root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"
check_dir=${CHECK_DIR:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/uqrl-constitution-checks-${UID}}
structure_bin=${STRUCTURE_BIN:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/uqrl-constitution-structure-${UID}}
export CHECK_DIR="$check_dir"

fail() {
  printf 'check: %s\n' "$*" >&2
  exit 2
}

require() {
  command -v "$1" >/dev/null 2>&1 || fail "required command not found: $1"
}

status_value() {
  local file=$1 key=$2
  awk -F= -v key="$key" '$1 == key {print substr($0, length(key) + 2); exit}' "$file"
}

plural() {
  [[ "$1" == 1 ]] || printf 's'
}

write_status() {
  local stage=$1 result=$2 errors=$3 warnings=$4
  mkdir -p "$check_dir"
  {
    printf 'result=%s\n' "$result"
    printf 'errors=%s\n' "$errors"
    printf 'warnings=%s\n' "$warnings"
  } > "$check_dir/$stage.status"
}

compile_structure() {
  local compiled
  require rustc
  compiled=$(mktemp "$structure_bin.XXXXXX")
  if ! rustc --edition=2021 -C opt-level=z -C lto=fat -C codegen-units=1 -C panic=abort -C strip=symbols tools/scripts/structure.rs -o "$compiled"; then
    rm -f -- "$compiled"
    return 1
  fi
  if ! "$compiled" selfcheck; then
    rm -f -- "$compiled"
    return 1
  fi
  mv -f -- "$compiled" "$structure_bin"
}

ensure_structure() {
  if [[ ! -x "$structure_bin" || tools/scripts/structure.rs -nt "$structure_bin" || tools/scripts/check.sh -nt "$structure_bin" ]]; then
    compile_structure
  fi
}

prepare() {
  local command
  for command in rustc jq awk; do
    require "$command"
  done
  mkdir -p "$check_dir"
  for stage in structure drafting language integrity pdf; do
    rm -f -- "$check_dir/$stage.status" "$check_dir/$stage.md" "$check_dir/$stage.log"
  done
  ensure_structure
  VALE_BIN=$("$root/tools/scripts/prose.sh" prepare)
  export VALE_BIN
  "$root/tools/scripts/prose.sh" selfcheck
}

annotate() {
  [[ "${GITHUB_ACTIONS:-}" == true ]] || return 0
  local severity=$1 diagnostic=$2 source_line=$3 text=$4 escaped level
  escaped=${text//'%'/'%25'}
  escaped=${escaped//$'\r'/'%0D'}
  escaped=${escaped//$'\n'/'%0A'}
  level=warning
  [[ "$severity" == ERROR ]] && level=error
  printf '::%s file=Constitution.md,line=%s::%s %s\n' "$level" "$source_line" "$diagnostic" "$escaped"
}

emit_annotations() {
  local log=$1 severity diagnostic source_line text
  while IFS=$'\t' read -r severity diagnostic source_line text; do
    [[ "$severity" == ERROR || "$severity" == WARN ]] || continue
    annotate "$severity" "$diagnostic" "$source_line" "$text"
  done < "$log"
}

finding_bullets() {
  local log=$1 limit=${2:-3} count=0 severity diagnostic source_line text total_findings
  while IFS=$'\t' read -r severity diagnostic source_line text; do
    [[ "$severity" == ERROR || "$severity" == WARN ]] || continue
    printf -- '- %s at line %s: %s\n' "\`$diagnostic\`" "$source_line" "$text"
    count=$((count + 1))
    [[ "$count" -lt "$limit" ]] || break
  done < "$log"
  total_findings=$(awk -F '\t' '$1 == "ERROR" || $1 == "WARN" {count++} END {print count + 0}' "$log")
  if [[ "$total_findings" -gt "$count" ]]; then
    printf -- '- And %s more; see logs.\n' "$((total_findings - count))"
  fi
}

stat_value() {
  local log=$1 key=$2
  awk -F '\t' -v key="$key" '
    $1 == "STAT" {
      count = split($4, values, ";")
      for (i = 1; i <= count; i++) {
        split(values[i], pair, "=")
        if (pair[1] == key) {
          print substr(values[i], length(key) + 2)
          exit
        }
      }
    }
  ' "$log"
}

run_structure() {
  local stage=$1
  shift
  local log="$check_dir/$stage.log" rc errors warnings result
  mkdir -p "$check_dir"
  ensure_structure
  set +e
  "$structure_bin" "$@" | tee "$log"
  rc=${PIPESTATUS[0]}
  set -e
  errors=$(awk -F '\t' '$1 == "ERROR" {count++} END {print count + 0}' "$log")
  warnings=$(awk -F '\t' '$1 == "WARN" {count++} END {print count + 0}' "$log")
  [[ "$rc" -eq 0 ]] || errors=$((errors + (errors == 0)))
  result=passed
  [[ "$rc" -eq 0 ]] || result=failed
  write_status "$stage" "$result" "$errors" "$warnings"
  emit_annotations "$log"
  case "$stage" in
    structure) write_structure_fragment "$log" "$errors" "$warnings" ;;
    drafting) write_drafting_fragment "$log" "$errors" "$warnings" ;;
    integrity) write_integrity_fragment "$log" "$errors" "$warnings" ;;
  esac
  return "$rc"
}

write_structure_fragment() {
  local log=$1 errors=$2 warnings=$3 headings clauses
  headings=$(stat_value "$log" headings)
  clauses=$(stat_value "$log" clauses)
  {
    printf -- '- %s section heading%s checked.\n' "${headings:-0}" "$(plural "${headings:-0}")"
    printf -- '- %s explicit legal clause%s detected.\n' "${clauses:-0}" "$(plural "${clauses:-0}")"
    if [[ "${clauses:-0}" == 0 ]]; then
      printf -- '- Canonical numbering is not yet present; heading and list hierarchy were validated.\n'
    fi
    printf -- '- %s error%s and %s warning%s.\n' \
      "$errors" "$(plural "$errors")" \
      "$warnings" "$(plural "$warnings")"
    finding_bullets "$log" 2
  } > "$check_dir/structure.md"
}

write_drafting_fragment() {
  local log=$1 errors=$2 warnings=$3
  {
    printf -- '- %s advisory finding%s.\n' "$warnings" "$(plural "$warnings")"
    printf -- '- %s blocking error%s.\n' "$errors" "$(plural "$errors")"
    finding_bullets "$log" 3
    [[ "$errors" != 0 || "$warnings" != 0 ]] || printf -- '- No targeted drafting issues found.\n'
  } > "$check_dir/drafting.md"
}

write_integrity_fragment() {
  local log=$1 errors=$2 warnings=$3 references undefined
  references=$(awk -F '\t' '$2 ~ /^REF/ {count++} END {print count + 0}' "$log")
  undefined=$(awk -F '\t' '$2 == "ROLE001" {count++} END {print count + 0}' "$log")
  {
    printf -- '- %s reference finding%s; %s undefined-office warning%s.\n' \
      "$references" "$(plural "$references")" \
      "$undefined" "$(plural "$undefined")"
    printf -- '- %s error%s and %s warning%s.\n' \
      "$errors" "$(plural "$errors")" \
      "$warnings" "$(plural "$warnings")"
    finding_bullets "$log" 2
  } > "$check_dir/integrity.md"
}

run_prose() {
  local stage=$1 command=$2 log rc
  log="$check_dir/$stage.log"
  mkdir -p "$check_dir"
  set +e
  "$root/tools/scripts/prose.sh" "$command" Constitution.md | tee "$log"
  rc=${PIPESTATUS[0]}
  set -e
  return "$rc"
}

pdf_needed() {
  local base head
  [[ "${GITHUB_REF:-}" != refs/tags/* ]] || return 0
  [[ "${FORCE_PDF:-}" != true ]] || return 0
  [[ "${GITHUB_ACTIONS:-}" == true ]] || return 0
  case "${GITHUB_EVENT_NAME:-}" in
    pull_request) base=${CHECK_BASE_SHA:-} ;;
    push) base=${CHECK_BEFORE_SHA:-} ;;
    *) return 0 ;;
  esac
  [[ "$base" =~ ^[0-9a-fA-F]{40}$ && "$base" != 0000000000000000000000000000000000000000 ]] || return 0
  require git
  git cat-file -e "$base^{commit}" 2>/dev/null || return 0
  head=${GITHUB_SHA:-HEAD}
  git cat-file -e "$head^{commit}" 2>/dev/null || return 0
  ! git diff --quiet "$base" "$head" -- Constitution.md tools/config/constitution.typ tools/scripts/render.sh .github/workflows/release.yml
}

run_pdf() {
  local log="$check_dir/pdf.log" rc pdf bytes kib digest
  mkdir -p "$check_dir"
  if ! pdf_needed; then
    write_status pdf skipped 0 0
    printf -- '- Skipped: no PDF-affecting files changed.\n' > "$check_dir/pdf.md"
    printf 'PDF check skipped: no PDF-affecting files changed.\n'
    return 0
  fi

  set +e
  "$root/tools/scripts/render.sh" dist | tee "$log"
  rc=${PIPESTATUS[0]}
  set -e
  if [[ "$rc" -ne 0 ]]; then
    write_status pdf failed 1 0
    printf -- '- PDF build failed; see the job log.\n' > "$check_dir/pdf.md"
    return "$rc"
  fi

  pdf=$(sed -n 's/^PDF: //p' "$log")
  [[ -s "$pdf" ]] || fail "rendered PDF is missing: $pdf"
  bytes=$(wc -c < "$pdf")
  kib=$(((bytes + 1023) / 1024))
  digest=$(awk '/^SHA-256:/ {print $2}' "$log")
  write_status pdf passed 0 0
  {
    printf -- '- Built %s.\n' "\`${pdf##*/}\`"
    printf -- '- Size: %s KiB.\n' "$kib"
    printf -- '- SHA-256: %s.\n' "\`$digest\`"
  } > "$check_dir/pdf.md"
}

stage_outcome() {
  case "$1" in
    structure) printf '%s\n' "${STRUCTURE_OUTCOME:-}" ;;
    drafting) printf '%s\n' "${DRAFTING_OUTCOME:-}" ;;
    language) printf '%s\n' "${LANGUAGE_OUTCOME:-}" ;;
    integrity) printf '%s\n' "${INTEGRITY_OUTCOME:-}" ;;
    pdf) printf '%s\n' "${PDF_OUTCOME:-}" ;;
  esac
}

display_result() {
  case "$1" in
    passed) printf '✅ Passed' ;;
    failed) printf '❌ Failed' ;;
    *) printf '⏭️ Skipped' ;;
  esac
}

summary() {
  local summary_file="$check_dir/summary.md" failed=0 stage label status result errors warnings outcome
  mkdir -p "$check_dir"
  : > "$summary_file"
  {
    printf '# Constitution checks\n\n'
    printf '| Check | Result | Errors | Warnings |\n'
    printf '|---|---:|---:|---:|\n'
    for stage in structure drafting language integrity pdf; do
      case "$stage" in
        structure) label=Structure ;;
        drafting) label='Drafting Style' ;;
        language) label='Spelling & Typos' ;;
        integrity) label=Integrity ;;
        pdf) label=PDF ;;
      esac
      status="$check_dir/$stage.status"
      if [[ -s "$status" ]]; then
        result=$(status_value "$status" result)
        errors=$(status_value "$status" errors)
        warnings=$(status_value "$status" warnings)
      else
        result=skipped
        errors=0
        warnings=0
      fi
      outcome=$(stage_outcome "$stage")
      if [[ "$outcome" == failure || "$outcome" == cancelled ]]; then
        result=failed
        [[ "$errors" -gt 0 ]] || errors=1
      fi
      [[ "$result" != failed ]] || failed=1
      printf '| %s | %s | %s | %s |\n' "$label" "$(display_result "$result")" "$errors" "$warnings"
    done
    for stage in structure drafting language integrity pdf; do
      case "$stage" in
        structure) label=Structure ;;
        drafting) label='Drafting Style' ;;
        language) label='Spelling & Typos' ;;
        integrity) label=Integrity ;;
        pdf) label=PDF ;;
      esac
      printf '\n## %s\n\n' "$label"
      if [[ -s "$check_dir/$stage.md" ]]; then
        cat "$check_dir/$stage.md"
      else
        printf -- '- Stage did not run.\n'
      fi
    done
  } >> "$summary_file"

  if [[ "${PREPARE_OUTCOME:-}" == failure || "${PREPARE_OUTCOME:-}" == cancelled ]]; then
    failed=1
  fi
  cat "$summary_file"
  if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
    cat "$summary_file" >> "$GITHUB_STEP_SUMMARY"
  fi
  [[ "$failed" == 0 ]]
}

all() {
  local failed=0
  prepare
  if ! run_structure structure structure Constitution.md; then failed=1; fi
  if ! run_structure drafting drafting Constitution.md; then failed=1; fi
  if ! run_prose language language; then failed=1; fi
  if ! run_structure integrity integrity Constitution.md; then failed=1; fi
  if ! run_pdf; then failed=1; fi
  if ! summary; then failed=1; fi
  [[ "$failed" == 0 ]]
}

selfcheck() {
  local temporary_bin
  temporary_bin=$(mktemp -d)
  trap 'rm -rf -- "$temporary_bin"' RETURN
  rustc --edition=2021 -C opt-level=z -C lto=fat -C codegen-units=1 -C panic=abort -C strip=symbols tools/scripts/structure.rs -o "$temporary_bin/structure"
  "$temporary_bin/structure" selfcheck
  "$root/tools/scripts/prose.sh" selfcheck
}

usage() {
  printf 'usage: check prepare|structure|drafting|language|integrity|pdf-needed|pdf|summary|all|selfcheck\n' >&2
  exit 2
}

case "${1:-}" in
  prepare) [[ "$#" == 1 ]] || usage; prepare ;;
  structure) [[ "$#" == 1 ]] || usage; run_structure structure structure Constitution.md ;;
  drafting) [[ "$#" == 1 ]] || usage; run_structure drafting drafting Constitution.md ;;
  language) [[ "$#" == 1 ]] || usage; run_prose language language ;;
  integrity) [[ "$#" == 1 ]] || usage; run_structure integrity integrity Constitution.md ;;
  pdf-needed) [[ "$#" == 1 ]] || usage; pdf_needed ;;
  pdf) [[ "$#" == 1 ]] || usage; run_pdf ;;
  summary) [[ "$#" == 1 ]] || usage; summary ;;
  all) [[ "$#" == 1 ]] || usage; all ;;
  selfcheck) [[ "$#" == 1 ]] || usage; selfcheck ;;
  *) usage ;;
esac
