# Real game win rates of deck pairs, from the Pauper MTGO Challenge matches of
# github.com/fnpavel/pauperdata (data/matchups/matches-YEAR.json, Kirblinxy's
# round-by-round records). Run by script/calibrate.sh real, slurping every
# year's file:
#   jq -n -r --arg from 2025-07-01 --argjson pairs '[["A","B"]]' \
#     -f script/calibration/real-rates.jq matches-2025.json matches-2026.json
# One line per pair: A, B, the games played between them, the games A won,
# and the matches, tab-separated. A match counts only when both players'
# records of it agree (pairing_quality "paired") and both game scores are
# known; a mirror never counts.
[inputs[]]
| map(select(.date >= $from and .pairing_quality == "paired" and .games_a != null and .games_b != null and .deck_a != .deck_b)) as $matches
| $pairs[] as [$a, $b]
| ([$matches[] | select(.deck_a == $a and .deck_b == $b) | [.games_a, .games_b]]
   + [$matches[] | select(.deck_a == $b and .deck_b == $a) | [.games_b, .games_a]]) as $scores
| [$a, $b, ($scores | map(.[0] + .[1]) | add // 0), ($scores | map(.[0]) | add // 0), ($scores | length)]
| @tsv
