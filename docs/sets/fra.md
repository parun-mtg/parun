# Reality Fracture census

The Reality Fracture (FRA) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-06 from MTGJSON's set list against `data/cards/`.
The set list is `data/decks/sets/fra/all.txt`, which the suite reads, so every
card of the set stays in the pool.

## Expressible today

Every expressible Reality Fracture card has been added to `data/cards/`.

## Needs a capability

Nothing. Every Reality Fracture card is in the pool.

## Decks

MTGJSON lists one FRA deck, Reality Fracture Redemption, an MTGO redemption
list rather than a playable deck, so `data/decks/sets/fra/` holds only the set
list.

## How it went

Finished 2026-11-08.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 31 | #6497 |
| Translated (widened translator) | 23 | #6540 (21), #6542 (2) |
| Hand-written | 188 | #6544 (20), #6550 (19), #6555 (17), #6557 (10), #6558 (12), #6559 (12), #6562 (11), #6571 (11), #6574 (11), #6578 (11), #6585 (12), #6603 (11), #6604 (11), #6613 (10), #6625 (10) |
| Behind a capability | 30, plus 0 from other sets | below |

- Census trend: the first dry run translated 31 of the 272 missing (9 FRA
  reprints were already in the pool); 67 needed a capability at the census, 35
  of them behind Empower Jace. Units ran in parallel, so the census was edited
  by many PRs at once; #6549 replaces that for the next set.
- Capability issues, census order (hardest first), then as filed: #6485
  Empower Jace (#6507, 3 cards; the rest translated or hand-written once it
  landed), #6493 Hall of Echoes (#6499), #6492 Draconic Visitor (#6503), #6486
  enters prepared (#6509, Pompous Battlemage; the other preparation cards
  followed), #6489 Chandra, Chill of Compliance and Enlightened Confidant
  (#6502), #6494 Tomik, Orzhov Lawmage (#6504), #6488 Codie, Ravenous Codex
  (#6506), #6487 Infinite Coursework (#6519), #6490 Desperate Futurescribe,
  Proctor of Potential and Surveillance Phantasm (#6501), #6491 Command the
  Stage (#6524), #6496 Loot, the Anomaly (#6505), #6495 Loot, the Nexus and
  Tam, the Possibility (#6500), #4295 Extrapolate the Impossible (#6510), #6526
  the Heartwood type (#6527, no card; its cards came in hand batches),
  #6457 Emergency Phytomedic and Void Extrapolator (#6545), #6546 Emrakul, the
  Exigent Doom (#6628), #6556 Garruk, Veiled Butcher (#6632), #6560 Mabel,
  Bitter Recluse (#6640), #6568 Omnipresence (#6635), #6572 Molten Tide
  (#6642), #6576 Sanctum Lurker (#6643), #6588 Theorist's Proxy (#6648), #6607
  Way of the Mind Sculptor (#6655), #6611 Warrior's Blades (#6657).
- Filed along the way: #6508 (Turtles Forever's half of #4295; open), #6543
  (a trigger's mode answer unchecked), #6551 (a failing scenario check hides
  earlier ones), #6573 (five permanent types standing in for nonland
  permanent), #6624 (a refused target choice passing on a misspelled slot).
- Audits: hand batches #6557, #6558, #6603 and #6625, each fixed on its own
  branch; capability PRs not recorded. 3 found rules defects:
  - #6557: an "is attacked" batch fired once for two attacked opponents, and
    `controllerTurnScoped` ignored `scope`.
  - #6558: a co-attacker was read live, not off the declaration.
  - #6603: behold offered a permanent entering alongside.
- Cost: not recorded. Wall clock ~44h, from the census PR's opening to the
  last capability PR's merge.
- Owner review:
  - #6506: decisions.md "What "a prepared spell" names".
  - Wire-format changes: #6519 Undesignate (Eliminate the Impossible and
    Deadly Complication rewritten), #6657 `CostModifier.perTarget` (Hinata,
    Dawn-Crowned and Battlefield Thaumaturge rewritten).
