# Reduces MTGJSON's AllPrintings.json to data/printings.json (#4118): every
# printing of a pool card, by set code and collector number, with its card's
# faces' names joined with " // " (Parun.Ingest.cardKey's key) and the numbers
# CR 718.1 says it lights, a meld card by its own face (cardKey). $pool is
# those keys, one per pool card file. Run as
#   jq -r --slurpfile pool <(jq '[.faces[].name] | join(" // ")' data/cards/*.json) \
#     -f script/ingest/printings.jq _scratch/AllPrintings.json > data/printings.json
# One printing a line, so a new set's diff names its printings.
def entry: {card} + (if (.lights | length) > 0 then {lights: (.lights | sort)} else {} end);
.meta.date as $date
| ($pool | map({(.): true}) | add) as $known
| [.data[] | .code as $set | .cards[] | (if .layout == "meld" then .faceName else .name end) as $name | select($known[$name]) | {set: $set, number, card: $name, lights: (.attractionLights // [])}]
| unique
| group_by(.set)
| map("    \(.[0].set | tojson): {\n" + (map("      \(.number | tojson): \(entry | tojson)") | join(",\n")) + "\n    }")
| "{\n  \"date\": \($date | tojson),\n  \"printings\": {\n" + join(",\n") + "\n  }\n}"
