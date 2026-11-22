#!/usr/bin/env bash
# Compares the bots' win rates with real ones, matchup by matchup (#9346).
# TABLE (data/calibration/pauper.tsv when absent) lists each matchup: two deck
# lists under data/decks, their archetypes' names in the source, and the real
# games, the first deck's game wins and the matches, after `#` lines saying
# where those came from.
#
#   script/calibrate.sh real [TABLE]
#     recounts each matchup's real games from the source its header names, at
#     the commit it pins, cloned into _scratch/pauperdata, and rewrites TABLE.
#   script/calibrate.sh run [-t TABLE] [-j JOBS] [-o DIR] [-- PARUN MATCH OPTION...]
#     plays every matchup with `parun match` and the options given (each game's
#     line and tally in DIR, _scratch/calibration when absent), JOBS at once (1
#     when absent), then prints a table of real against simulated game win
#     rates, over games won (a draw or a stop counts in neither), each with a
#     95% Wilson interval, and each deck's mean signed gap.
#     PARUN names the command to run (`script/with-build-lock.sh cabal run -v0
#     parun --` when unset), so an optimized build can play the games.
set -euo pipefail
default=data/calibration/pauper.tsv
usage() { sed -n '2,18p' "$0" >&2; exit 2; }
[ $# -ge 1 ] || usage
mode=$1
shift

rows() { grep -v '^#' "$1" | grep -v '^$'; }

case $mode in
  real)
    table=${1:-$default}
    repo=$(sed -n 's/^# repository: //p' "$table")
    commit=$(sed -n 's/^# commit: //p' "$table")
    from=$(sed -n 's/^# from: //p' "$table")
    clone=_scratch/pauperdata
    [ -d "$clone/.git" ] || git clone --quiet "$repo" "$clone"
    git -C "$clone" fetch --quiet origin "$commit" 2>/dev/null || true
    git -C "$clone" checkout --quiet "$commit"
    pairs=$(rows "$table" | cut -f3,4 | jq -R -s -c 'split("\n") | map(select(. != "") | split("\t"))')
    real=$(jq -n -r --arg from "$from" --argjson pairs "$pairs" -f script/calibration/real-rates.jq "$clone"/data/matchups/matches-*.json)
    out=$(mktemp)
    grep '^#' "$table" > "$out"
    paste <(rows "$table" | cut -f1,2) <(printf '%s\n' "$real") >> "$out"
    mv "$out" "$table"
    ;;
  run)
    table=$default
    jobs=1
    dir=_scratch/calibration
    while getopts t:j:o: flag; do
      case $flag in
        t) table=$OPTARG ;;
        j) jobs=$OPTARG ;;
        o) dir=$OPTARG ;;
        *) usage ;;
      esac
    done
    shift $((OPTIND - 1))
    read -r -a parun <<< "${PARUN:-script/with-build-lock.sh cabal run -v0 parun --}"
    mkdir -p "$dir"
    n=0
    while IFS=$'\t' read -r one two _; do
      n=$((n + 1))
      "${parun[@]}" match "$@" "data/decks/$one" "data/decks/$two" > "$dir/$n.out" 2> "$dir/$n.err" &
      while [ "$(jobs -rp | wc -l)" -ge "$jobs" ]; do wait -n; done
    done < <(rows "$table")
    wait
    n=0
    while IFS=$'\t' read -r one two _ _ games wins matches; do
      n=$((n + 1))
      won=$(sed -n 's/^Seat [12] won \([0-9]*\) of [0-9]* games\.$/\1/p' "$dir/$n.out" | paste -s -d ' ')
      [[ $won =~ ^[0-9]+\ [0-9]+$ ]] || { echo "$one against $two: no tally in $dir/$n.out" >&2; exit 1; }
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$one" "$two" "$games" "$wins" "$matches" "$won"
    done < <(rows "$table") | awk -F'\t' '
      function wilson(k, n,  p, z, c, h) {
        if (n == 0) return "-"
        z = 1.96; p = k / n
        c = (p + z * z / (2 * n)) / (1 + z * z / n)
        h = z * sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n)
        return sprintf("%.3f [%.3f, %.3f]", p, (c - h < 0) ? 0 : c - h, (c + h > 1) ? 1 : c + h)
      }
      function name(path) { sub(/^.*\//, "", path); sub(/\.txt$/, "", path); return path }
      {
        split($6, s, " "); decided = s[1] + s[2]
        real = $4 / $3; sim = (decided > 0) ? s[1] / decided : 0
        gap = sim - real
        agree = ((real - 0.5) * (sim - 0.5) > 0) ? "yes" : "no"
        printf "| %s | %s | %s (%d games) | %s (%d games) | %+.3f | %s |\n", name($1), name($2), wilson($4, $3), $3, wilson(s[1], decided), decided, gap, agree
        gaps[name($1)] += gap; count[name($1)]++
        gaps[name($2)] -= gap; count[name($2)]++
        agreed += (agree == "yes"); rows++
        r[rows] = real; m[rows] = sim
      }
      BEGIN {
        print "| Deck | Against | Real | Simulated | Gap | Same side of 50% |"
        print "| --- | --- | --- | --- | --- | --- |"
      }
      END {
        for (i = 1; i <= rows; i++) for (j = i + 1; j <= rows; j++) {
          d = (r[i] - r[j]) * (m[i] - m[j]); pairs++
          if (d > 0) concordant++; else if (d < 0) discordant++
        }
        printf "\n%d of %d matchups on the same side of 50%%; of %d pairs of matchups, %d ordered alike and %d reversed.\n\n", agreed, rows, pairs, concordant, discordant
        print "| Deck | Mean signed gap | Matchups |"
        print "| --- | --- | --- |"
        for (d in gaps) printf "| %s | %+.3f | %d |\n", d, gaps[d] / count[d], count[d]
      }'
    ;;
  *) usage ;;
esac
