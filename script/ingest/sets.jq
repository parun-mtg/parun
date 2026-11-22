# Reduces MTGJSON's AllPrintings.json to data/decks/sets.json (#8720): the
# name and release date of each set data/decks/sets holds a directory for,
# by its code, which the web client's deck browser shows a precon's set by.
# Run as
#   jq --sort-keys --argjson codes "$(ls data/decks/sets | jq -R 'ascii_upcase' | jq -sc .)" \
#     -f script/ingest/sets.jq _scratch/AllPrintings.json > data/decks/sets.json
.data
| with_entries(select(.key as $code | $codes | index($code)) | .value |= {name, releaseDate})
