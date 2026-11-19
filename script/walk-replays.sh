#!/usr/bin/env bash
# Walks scenarios through the web client's replay (?scenario=…) headlessly,
# over WebDriver, and saves a screenshot of every position as each seat sees
# it: OUT/NAME/POSITION-SEAT.png, the same bytes each run, so two runs (before
# and after a change) compare file by file. With no arguments, every scenario in
# data/scenario-manifest.json; else each argument is a theme of it or a
# scenario's name. Serve the client first (script/serve-web.sh), and allow
# remote automation in Safari Technology Preview once (its Develop menu, or
# `safaridriver --enable`):
#   script/walk-replays.sh [-u URL] [-o OUT] [-d SAFARIDRIVER] [-p PORT] [THEME|NAME]...
set -euo pipefail
url=https://localhost:8443/
out=_scratch/replays
driver="/Applications/Safari Technology Preview.app/Contents/MacOS/safaridriver"
port=4444
while getopts u:o:d:p: flag; do
  case $flag in
    u) url=$OPTARG ;;
    o) out=$OPTARG ;;
    d) driver=$OPTARG ;;
    p) port=$OPTARG ;;
    *) sed -n '2,10p' "$0" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
manifest=data/scenario-manifest.json
if [ $# -eq 0 ]; then
  mapfile -t names < <(jq -r '.[][]' "$manifest")
else
  names=()
  for wanted in "$@"; do
    mapfile -t -O "${#names[@]}" names < <(jq -r --arg t "$wanted" 'if has($t) then .[$t][] else $t end' "$manifest")
  done
fi

"$driver" --port "$port" >/dev/null 2>&1 &
driving=$!
base=http://localhost:$port
session=
finish() {
  [ -n "$session" ] && curl -s -X DELETE "$base/session/$session" >/dev/null
  kill "$driving" 2>/dev/null || true
}
trap finish EXIT
for _ in $(seq 50); do curl -sf "$base/status" >/dev/null && break; sleep 0.1; done

# A WebDriver call: METHOD PATH [BODY], answering its value.
call() {
  curl -sf -X "$1" "$base/session/$session$2" -H 'Content-Type: application/json' ${3:+-d "$3"} | jq -c '.value'
}
# A script run in the page, answering what it returns.
run() {
  call POST /execute/sync "$(jq -nc --arg s "$1" '{script: $s, args: []}')"
}
# Runs a script until it returns something other than null, for up to 30 s.
until_shown() {
  local got
  for _ in $(seq 300); do
    got=$(run "$1")
    [ "$got" != null ] && { echo "$got"; return; }
    sleep 0.1
  done
  echo "timed out waiting on: $1" >&2
  return 1
}

session=$(curl -sf -X POST "$base/session" -H 'Content-Type: application/json' \
  -d '{"capabilities": {"alwaysMatch": {"browserName": "Safari Technology Preview"}}}' | jq -r '.value.sessionId')
call POST /window/rect '{"width": 1440, "height": 900}' >/dev/null

# The replay's position, its last, and its seats once it shows; or the
# message a page that is not a replay shows instead.
state='const m = document.querySelector("#app main");
if (m?.classList.contains("replay")) {
  const r = m.querySelector(".scrubber input");
  return {at: +r.value, last: +r.max, seats: [...m.querySelectorAll(".seats button")].map((b) => [b.textContent, b.getAttribute("aria-pressed") === "true"])};
}
return m && !m.textContent.includes("Loading") && !m.querySelector(".spinner") ? {message: m.textContent} : null;'

for name in "${names[@]}"; do
  echo "$name"
  mkdir -p "$out/$name"
  call POST /url "$(jq -nc --arg u "$url?scenario=$name&at=0" '{url: $u}')" >/dev/null
  shown=$(until_shown "$state")
  if [ "$(jq -r 'has("message")' <<<"$shown")" = true ]; then
    echo "  $(jq -r '.message' <<<"$shown")" >&2
    call GET /screenshot | jq -r . | base64 -d >"$out/$name/message.png"
    continue
  fi
  last=$(jq -r '.last' <<<"$shown")
  seats=$(jq -r '.seats | length' <<<"$shown")
  for at in $(seq 0 "$last"); do
    for seat in $(seq 0 $((seats - 1))); do
      run "document.querySelectorAll(\"#app .seats button\")[$seat].click(); document.activeElement?.blur(); return null;" >/dev/null
      # Motion reduced and every animation ended, so the same position
      # screenshots the same way each run.
      shown=$(until_shown "document.documentElement.dataset.motion = 'reduced'; const s = (() => { $state })(); return s?.seats?.[$seat]?.[1] && s.at === $at && document.getAnimations().every((a) => a.playState !== 'running') ? s : null;")
      label=$(jq -r ".seats[$seat][0]" <<<"$shown")
      call GET /screenshot | jq -r . | base64 -d >"$out/$name/$(printf %03d "$at")-$label.png"
    done
    if [ "$at" -lt "$last" ]; then
      run 'document.querySelector("#app [aria-label=\"Next position\"]").click(); return null;' >/dev/null
      until_shown "const s = (() => { $state })(); return s?.at === $((at + 1)) ? s : null;" >/dev/null
    fi
  done
done
echo "Saved to $out."
