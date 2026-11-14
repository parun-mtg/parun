#!/usr/bin/env bash
# Wait on issues dispatched with the `claude` label. Usage:
#   script/wait-issues.sh ISSUE...
# Polls every four minutes and exits 0 as soon as one issue closes ("CLOSED
# N") or, still open, loses the `claude` label ("UNLABELLED N"): either way
# the orchestrator has something to read. A failed reading is skipped and
# retried on the next round.
set -euo pipefail
[ "$#" -ge 1 ] || { echo "usage: $0 ISSUE..." >&2; exit 2; }
while :; do
  for n in "$@"; do
    reading=$(gh issue view "$n" --json state,labels --jq '.state + " " + ([.labels[].name] | join(","))' 2>/dev/null) || continue
    case "$reading" in
      CLOSED*) echo "CLOSED $n"; exit 0 ;;
      *claude*) ;;
      *) echo "UNLABELLED $n ($reading)"; exit 0 ;;
    esac
  done
  sleep 240
done
