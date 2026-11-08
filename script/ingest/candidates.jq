# Reduces MTGJSON's AtomicCards.json to what `parun ingest` reads (#9): every
# single-faced card as a record it may build, and every preparation card (CR
# 723) as a record of its faces' records, which it leaves out; each with the
# sets it was printed in for `parun translate --set`; every face's Oracle
# text by card and face name, which both stamp on the cards they write; and
# every card's legalities by its faces' names, as Parun.Ingest.cardKey joins
# them, which `parun ingest` writes to data/legalities.json for the pool. A
# reversible card is a second printing of a card, so it is left out. Run as
#   jq -f script/ingest/candidates.jq _scratch/AtomicCards.json > _scratch/candidates.json
def printed: {manaCost, colorIndicator, supertypes, types, subtypes, power, toughness, text};
{
  version: .meta.version,
  date: .meta.date,
  cards: [
    .data[]
    | select(all(.[]; (.isFunny // false) == false))
    | if length == 1 and .[0].layout == "normal" then
        .[0] | {name} + printed + {printings}
      elif length > 1 and all(.[]; .layout == "prepare") then
        {name: .[0].name, faces: [sort_by(.side)[] | {name: .faceName} + printed], printings: .[0].printings}
      else
        empty
      end
  ],
  legalities: [.data[] | select(.[0].layout != "reversible_card") | {card: ([sort_by(.side)[] | (.faceName // .name)] | join(" // ")), legalities: .[0].legalities}],
  texts: [.data | to_entries[] | .key as $card | .value[] | {card: $card, name: (.faceName // .name), text: (.text // "")}] | unique
}
