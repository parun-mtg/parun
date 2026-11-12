# Teenage Mutant Ninja Turtles census

The Teenage Mutant Ninja Turtles (TMT) cards not yet in the pool, and which
wait on a capability. Derived 2026-11-12 from MTGJSON's set list against
`data/cards/` (#7056). The set list is `data/decks/sets/tmt/all.txt`, which the
suite reads, so every card of the set stays in the pool.

TMT has 195 cards (MTGJSON); 19 were already in the pool, 176 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#7069) | 32 |
| Batches #7062 to #7068 | 139 |
| Capability issues #7058 to #7061 | 5 |

- Trend: the first dry run translated 32 of the 176 missing; 5 needed a
  capability.

## Decks

MTGJSON lists four theme decks (Donatello, Leonardo, Michelangelo, Raphael),
an Enemy Deck and three land packs. None is committed: decks are optional, so
`data/decks/sets/tmt/` holds only the set list. The set has no multi-faced
card, so every line is a plain card name.

## How it went

Finished 2026-11-12.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 32 | #7069 |
| Batches | 138 | #7074 (19), #7076 (20), #7079 (20), #7078 (20), #7077 (20), #7084 (20), #7085 (19) |
| Behind a capability | 6, plus 1 from other sets | below |

- Census trend: the first dry run translated 32 of the 176 missing; 5 needed
  a capability. Batch 1 overturned one verdict: Broadcast Takeover needed a
  control change that binds what it took (#7071).
- Capability issues, hardest first, then as filed: #7058 Leonardo, Sewer
  Samurai and Mikey & Don, Party Planners (#7091), #7059 Raph & Leo, Sibling
  Rivals (#7088), #7060 Michelangelo's Technique (#7089), #7061 Shredder's
  Armor (#7092), #7071 Broadcast Takeover (#7090, adding Mob Rule).
- Filed along the way: #7070 (Novel Nunchaku's fight read the attach target,
  #7086), #7072 (Raphael, Most Attitude's two attack triggers shared one play,
  #7082), #7073 (a sweep's filter left out of the slots an effect reads,
  #7081), #7075 (a second "may" when one looked-at card qualifies; open,
  Cowabunga!), #7083 (Mind Into Matter cast free, unverified; open, outside
  the set), #7093 (two encodings of a total cap; open), #7094 (an offered cast
  under an unrelated static permission, #7095).
- Three batches changed the engine: #7078 (sneak read where the card lies),
  #7079 (`LastKnown.castUsing`) and #7084 (`EntryAttack.ThatPlayer`). #7087
  then kept batch units to card data.
- Process changes during the set: #7057 (each unit logs a `Cost:` line),
  #7080 (`script/wait-pr.sh`; `data/legalities.json` left to the finish
  unit), #7087 (above).
- Audits: 4 of 5 capability PRs, 2 of 7 batch PRs and all 4 fix PRs; 1 found
  a rules defect. Skipped the census, the card-data and renderer-only batches
  (#7074, #7076, #7077, #7085), batch 6 (#7084, one constructor only Shark
  Shredder reaches) and the three-line capability PR #7092, each recorded on
  #7055.
  - #7091: a cast an effect offers could be made under an unrelated static
    permission and take its rider (#7094, fixed by #7095).
- Cost: 1325 turns, 229.9M cache read, 3.24M cache write, 1095k output over
  the 16 unit PRs that recorded one (#7074, #7076 to #7079, #7081, #7082,
  #7084 to #7086, #7088 to #7092, #7095), 763 session-minutes in all. Not
  recorded: the census (#7069). The finish unit's is on its own PR. Wall
  clock ~5.5h, from the census PR's opening to the last fix PR's merge
  (#7095), then the finish.
- Owner review:
  - #7091: decisions.md "Does a permission's entry rider outlive its
    source?" (a cast permission's entry-counter rider is fixed at cast).
  - #7095: reversed the reading #4050 introduced, that a cast an effect
    offers may be made under a static permission. Forced by CR 608.2g and
    the timing rulings, so no decisions entry.
  - Wire-format additions: #7079 (`LastKnown.castUsing`), #7082
    (`PermissionLimit.OncePerEffect`), #7084 (`EntryAttack.ThatPlayer`),
    #7086 (`Fight.hop`), #7088 (`Quantity.PhasesBegunThisTurn`), #7089
    (`ChosenCardFromAmong.total`), #7090 (`GainControl.slot`), #7091
    (`CastFromZone.entersWith`), #7092 (`Equip.timesEachTurn`).
