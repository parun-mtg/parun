#!/usr/bin/env bash
# Runs a cabal command, and runs it once more if it failed on ghc-pkg's
# package-db race (#10060): one unit re-registering in place removes its old
# .conf before writing the new one, and a ghc-pkg reading the database in that
# window fails on the missing file. A restored dist-wasm/ makes it likely,
# since every inplace unit re-registers at once. Any other failure is final.
# Usage:
#   script/retry-package-db-race.sh wasm32-wasi-cabal build all ...
set -euo pipefail
log=$(mktemp)
trap 'rm -f "$log"' EXIT
status=0
"$@" 2>&1 | tee "$log" || status=$?
if [ "$status" -ne 0 ] && grep -q 'packagedb/.*\.conf: getModificationTime:getFileStatus: does not exist' "$log"; then
  echo "$0: retrying after ghc-pkg's package-db race (#10060)" >&2
  status=0
  "$@" || status=$?
fi
exit "$status"
