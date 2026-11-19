# Phyrexia: All Will Be One census

Phyrexia: All Will Be One (ONE) cards not yet in the pool, and which wait on
a capability. Derived 2026-11-18 from MTGJSON's set list (AllPrintings
5.3.0+20261117) against `data/cards/` at f047407c (census #8736, tracker
#8735). The set list is `data/decks/sets/one/all.txt`, which the suite reads,
so every card of the set stays in the pool.

ONE has 271 card names, all single-faced and normal layout; 226 were missing.

| Place | Cards |
| --- | --- |
| Translated (census PR) | 34 |
| Batches #8754 through #8763 | 186 |
| Capability issues #8748 through #8753 | 6 |

- Trend: the first dry run translated 34 of the 226 missing; 6 needed a
  capability.
- Every planeswalker was hand-written (#5711).
- Blocked-by: #8756 (Feed the Infection) wrote the first `ForEach` over a
  `whose`-filtered player set feeding a life loss, so #8760 (Phyrexian Atlas)
  was linked as blocked by it; #8752 was linked as blocked by #8749 (both
  extend a counters event), and #8749 as blocked by FRC's #8740 and #8745
  (all extend `TriggerCondition`).
- No keyword-only card was missing.

## Decks

None is committed: decks are optional, so `data/decks/sets/one/` holds only
the set list, 271 names.

## How it went

Finished 2026-11-19.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 34 | #8764 |
| Batches | 184 | #8769 (19), #8768 (19), #8772 (19), #8771 (18), #8781 (19), #8783 (19), #8776 (18), #8777 (18), #8785 (18), #8787 (17) |
| Behind a capability | 7, plus 0 from other sets | below |

- Census trend: the first dry run translated 34 of the 226 missing; 6 needed
  a capability. Batch 4 sent Green Sun's Twilight to a capability issue it
  filed (#8770). White Sun's Twilight, in batch 10's list, landed first with
  FRC's batch 2 (#8780).
- Capability issues, hardest first: #8748 Melira, the Living Cure (#8800),
  #8749 All Will Be One (#8812), #8750 Mirran Safehouse (#8796), #8751
  Goliath Hatchery (#8804), #8752 Churning Reservoir (#8817), #8753 Skrelv,
  Defector Mite (#8803), then the batch-filed #8770 Green Sun's Twilight
  (#8798, a false gap: card data only, a CR 608.2d either-or).
- Filed along the way: #8775 by batch 5 (#8781), Kaito, Dancing Shadow
  shipped gated, stricter than printed (#8794); #8806 a Two-Headed Giant
  recipient-order reading, off-tracker for the owner.
- Fix and proof issues: #8805 Melira raised a poison event already cut to
  zero (#8810), #8815 All Will Be One's putter check and per-player joining
  were unproven (#8816, which found proliferate split player counters into
  one event per kind, CR 701.34a).
- Audits: 5 of 8 capability PRs, by subagent; 1 found a rules defect.
  Reviewed inline: #8796, #8798 and #8803, clean, and both fixes. Skipped the
  census and all ten batches (card data, scenarios, renderer wording and
  lint-table entries). Each is recorded on #8735.
  - #8800: Melira applied to a poison event Vorinclex halved to zero (CR
    616.1f), fixed by #8805.
  - #8812: no defect; two proof gaps, filed #8815, whose fix found the
    proliferate defect.
  - #8794, #8804, #8817: clean. #8804 left one stale comment in
    `viewOfCharacteristics` to the next unit editing it.
- Cost: 3569 turns, 427.5M cache read, 5.04M cache write, 1884k output over
  all 21 unit PRs, 473 session-minutes in all. The finish unit's is on its
  own PR. Wall clock ~5.7h, from the tracker's opening (2026-11-18 19:49 UTC)
  to the last unit's merge (#8817, 2026-11-19 01:33 UTC), then the finish.
- `data/legalities.json`: LEGALITIES
- Owner review:
  - #8794: a `docs/decisions.md` entry (a Chain Veil grant beside Kaito's
    twice-each-turn grant gives three activations), and a wire-format
    change, `LoyaltyClause`'s codec made tagged (four cards respelled).
  - #8800: wire-format change, `DamageRewrite.SetAmount` folded into `Scale
    (Exactly q)` (three cards respelled); `CounterPattern.whichPlayerKind`
    and `CounterR.barsAdditional` added.
  - #8796: wire-format change, `LentCards.InYourGraveyard` renamed
    `InGraveyards` (Thranduil, the Elvenking respelled).
  - #8817: wire-format change, `EventShape.CountersPut` and
    `Filter.CountersPutThisTurn` became `CountersMoved` and
    `CountersMovedThisTurn` (seven cards respelled).
  - Additive wire shapes, no respelling: #8812 (`CounterHolder.Permanent`'s
    player relation, tag `PermanentOrPlayer`), #8804
    (`Measure.TotalToxicValue`).
