# Stardates census

Stardates (SDS) cards not yet in the pool, and which wait on a capability.
Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117) against
`data/cards/` at 2e59028b (census #8512, tracker #8510). The set list is
`data/decks/sets/sds/all.txt`, which the suite reads, so every card of the set
stays in the pool. SDS was released 2026-11-13.

SDS was censused together with Star Trek Commander (TRC,
`docs/sets/trc.md`, which holds the shared trend and blocked-by notes); the
sets share no card name. SDS has 30 card names, all single-faced normal
reprints; 21 were missing.

| Place | Cards |
| --- | --- |
| Translated (census PR) | 3 |
| Batches #8525 through #8534 | 18 |

## Decks

None is committed: decks are optional, so `data/decks/sets/sds/` holds only
the set list, 30 names.

## How it went

Finished 2026-11-18. SDS ran on TRC's tracker; the record of both sets, with
audits, cost and owner review, is `docs/sets/trc.md`'s "How it went".

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 3 | #8547 |
| Batches | 17 | among TRC's batches, `docs/sets/trc.md` |
| Behind a capability | 1 | #8580 Ertai Resurrected (#8699) |

- Census trend: no SDS card needed a capability. Batch 3 (#8527) sent
  Ertai Resurrected to #8580: "its controller draws" found no controller for
  a countered ability.
