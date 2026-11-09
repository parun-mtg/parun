# The Hobbit census

The Hobbit (HOB) cards not yet in the pool, and which wait on a capability.
Derived 2026-11-08 from MTGJSON's set list against `data/cards/` (#6688). The
set list is `data/decks/sets/hob/all.txt`, which the suite reads, so every card
of the set stays in the pool.

HOB has 193 cards (MTGJSON); 19 were already in the pool, 174 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#6689) | 23 |
| Batches #6697 to #6704 | 143 |
| Capability issues #6690 to #6696 | 7 |
| Behind an open issue elsewhere (#5296) | 1 |

- Trend: the first dry run translated 23 of the 174 missing; 8 needed a
  capability.

## Decks

MTGJSON lists five Welcome Decks, two box sets (Crack the Plates, Treasures
of Smaug) and three land packs. None is committed: decks are optional, so `data/decks/sets/hob/` holds only the set list. It writes
each Adventure card by MTGJSON's joined name, which resolves.

## How it went

Finished 2026-11-09.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 23 | #6689 |
| Batches | 141 | #6707 (18), #6712 (18), #6709 (17), #6715 (18), #6714 (18), #6716 (18), #6718 (17), #6713 (17) |
| Behind a capability | 10, plus 2 from other sets | below |

- Census trend: the first dry run translated 23 of the 174 missing; 8 needed
  a capability. Batches overturned two verdicts: Elrond, Moon-Reader (batch 3,
  #6709) and The Great Goblin (batch 7, #6718).
- Capability issues, census order (hardest first), then as filed: #6690
  Gollum, Riddle Master (#6746), #6691 Head of the Hunt (#6742), #6692
  Radagast of Rhosgobel (#6741), #6693 Thranduil, the Elvenking (#6743),
  #6694 Desert Were-Worm (#6744), #6695 Getaway Barrel (#6736), #6696 Azog,
  Moria's Ruin (#6732), #5296 Inside Information (#6745 from elsewhere, adding
  Nashi, Moon Sage's Scion; Inside Information deferred to #6757, added by
  #6761), #5733 Elrond, Moon-Reader (#6752, adopted from elsewhere), #6717
  The Great Goblin (#6727, adding Generous Patron).
- Filed along the way: #6717 (The Great Goblin's counter trigger of any
  kind), #6757 (Inside Information, deferred by #6745), #6763 and #6764
  (audit fixes).
- First set run under #6549's procedure. 2 of the set's PRs merged `main` in
  (#6715, #6736).
- Audits: 2 of 10 capability PRs; 1 found a rules defect.
  - #6744: Desert Were-Worm's first time each turn wasn't counted as reading
    attack history, so loop detection could refuse a loop a lap early (#6763,
    fixed by #6769).
  - #6752: no rules defect; a text-client regression and an uncited
    decisions entry (#6764, fixed by #6768).
- Cost: not recorded. Wall clock ~20h, from the census PR's opening to the
  last card's merge (#6761).
- Owner review:
  - #6744: decisions.md "What a short attack spends of Desert Were-Worm's
    first time".
  - #6752: decisions.md "Whether copying an activation that never reached
    the stack copies anything".
  - Wire-format change: #6741 `CastAsThoughItHadFlash` payload becomes
    `{matching, onlyFirst}` (8 pool cards rewritten).
