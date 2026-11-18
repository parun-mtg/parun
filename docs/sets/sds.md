# Stardates census

Stardates (SDS) cards not yet in the pool, and which wait on a capability.
Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117) against
`data/cards/` at 2e59028b, in one census with Star Trek Commander
(`docs/sets/trc.md`, which holds the shared figures). The set list is
`data/decks/sets/sds/all.txt`, which the suite reads, so every card of the set
stays in the pool. SDS was released 2026-11-13.

SDS has 30 card names, all single-faced and all reprints, none shared with
TRC; 21 were missing.

| Place | SDS |
| --- | --- |
| Added by the census (#8547) | 3 |
| Batches #8525 through #8534 | 18 |
| Capability issues | 0 |

## Decks

None is committed: `data/decks/sets/sds/` holds only the set list, 30 names.

## How it went

Finished 2026-11-18, with TRC; `docs/sets/trc.md` has the run's PRs,
audits, cost and owner review.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 3 | #8547 |
| Batches | 17 | #8566 (1), #8583 (4), #8631 (1), #8648 (2), #8630 (2), #8632 (1), #8645 (3), #8647 (3) |
| Behind a capability | 1 | #8699 |

- Census trend: no SDS card needed a capability at the census. Batch 3
  sent Ertai Resurrected to #8580: "its controller draws" found no
  controller for a countered ability, landed by #8699.
