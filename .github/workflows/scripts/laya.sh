#!/usr/bin/env bash
set -euo pipefail

trap 'printf "::warning::Laya advisory scan did not complete; PDF publication still uses the deterministic checks\n" >&2' ERR
venv="${RUNNER_TEMP:?RUNNER_TEMP is required}/laya"
python3 -m venv "$venv"
"$venv/bin/python" -m pip install --disable-pip-version-check --quiet 'torch==2.14.0' --index-url https://download.pytorch.org/whl/cpu
"$venv/bin/python" -m pip install --disable-pip-version-check --quiet 'laya==0.3.11'
"$venv/bin/python" .github/workflows/scripts/analyse.py
