# Star Trek Commander census

Star Trek Commander (TRC) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117)
against `data/cards/` at 2e59028b (census #8512, tracker #8510). The set list
is `data/decks/sets/trc/all.txt`, which the suite reads, so every card of the
set stays in the pool. TRC was released 2026-11-13.

TRC was censused together with its companion Stardates (SDS,
`docs/sets/sds.md`); the sets share no card name. TRC has 336 card names, all
single-faced (332 normal, 4 saga); 244 were missing.

| Place | Cards |
| --- | --- |
| Translated (census PR) | 45 |
| Batches #8525 through #8534 | 180 |
| Batch #8546 (Battlefront, blocked by #8542) | 6 |
| Capability issues #8535 through #8545 | 13 |

- Trend, both sets together: the first dry run translated 48 of the 265
  missing; 19 needed a capability (13 in the capability issues, 6 in #8546).
- Blocked-by: shapes new to the pool were each first written by one batch
  (typed cycling dual and Parley by #8525, reveal an X-or-Y land by #8526,
  the Klingon-or-Warrior pair by #8528, enters tapped unless two or more
  opponents by #8530); the batches reusing them were linked as blocked by
  those. #8541 was linked as blocked by #8537 (both extend
  `Parun.Types.Filter`), #8543 by #8542 (both extend `Parun.Types.Quantity`).
- No keyword-only card was missing.

## Decks

None is committed: decks are optional, so `data/decks/sets/trc/` holds only
the set list, 336 names.

## How it went

Finished 2026-11-18. The record covers both sets, since they ran as one
tracker; `docs/sets/sds.md` has SDS's share.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 48 (TRC 45, SDS 3) | #8547 |
| Batches | 200 (TRC 183, SDS 17) | #8566 (20), #8589 (19), #8583 (19), #8631 (20), #8648 (20), #8630 (20), #8632 (20), #8645 (19), #8647 (19), #8653 (18), #8674 (6, batch 11) |
| Behind a capability | 17 (TRC 16, SDS 1), plus 0 from other sets | below |

- Census trend: the first dry run translated 48 of the 265 missing; 19
  needed a capability. Batches sent four more to capabilities they filed:
  Combat Celebrant (batch 2, #8575), Ertai Resurrected (SDS, batch 3,
  #8580), Shared Animosity (batch 8, #8643) and The Guardian of Forever
  (batch 9, #8637). Batch 11 (#8546), the six Battlefront cards, waited on
  #8542 and landed as card data.
- Capability issues, hardest first: #8535 World at War (#8667), #8536 Return
  to Dust (#8666), #8537 Kang, Suspicious Captain and Koloth, Tribble Hunter
  (#8677), #8538 Sulu, Heroic Helmsman (#8675), #8539 Colossal Clone Master
  (#8679), #8540 Chief Montgomery Scott (#8672), #8541 Cmdr. Troi, Outgoing
  Empath (#8692), #8542 Korok, Liberated Rage and Kurn, Son of Mogh (#8663),
  #8543 Guinan, Ancient Advisor (#8673), #8544 The Immortal Sun (#8671),
  #8545 Pavel Chekov, First Posting (#8676), then the batch-filed #8575
  Combat Celebrant (#8689), #8580 Ertai Resurrected (#8699), #8637 The
  Guardian of Forever (#8698), #8643 Shared Animosity (#8704).
- Fix and proof issues: #8678 World at War's trigger fired at a later combat
  when its added combat was skipped (#8681), #8683 Sulu tripled his power
  stationing a Planet, not only a Spacecraft (#8686), #8690 scenarios proving
  Kang's control-bound goad and `IsGoaded` (#8701).
- Filed along the way: #8579 (Command Chair paying for its commander on the
  stack; closed not planned, batch 2 read the rulings the other way), #8576
  (three out-of-set Spacecraft leave a last-striation ability ungated; open,
  off the tracker).
- Audits: 3 of 15 capability PRs, by subagent; 2 found rules defects, the
  third surviving mutations. Reviewed inline: #8666, #8671, #8672, #8679 and
  #8689, clean. Skipped the census, the 11 batches, the other 7 capability
  PRs, both fixes and #8701 (card data, renderer wording, lint-table entries,
  small additive arms only the new card reads, or the audited finding fixed
  in place), each recorded on #8510.
  - #8667: World at War's trigger fired at a later combat when its added
    combat was skipped, fix #8678.
  - #8675: Sulu's tripling applied to stationing Planets too, fix #8683.
  - #8677: no defect; two mutations survived, proved by #8690.
- Cost: 3634 turns, 459.5M cache read, 5.40M cache write, 1914k output over
  29 of the 30 unit PRs, 696 session-minutes in all; #8675 (Sulu) carries no
  `Cost:` line, so its share is not recorded. The finish unit's is on its own
  PR. Wall clock ~9.7h, from the tracker's opening (2026-11-18 01:54 UTC) to
  the last unit's merge (#8704, 11:35 UTC), then the finish.
- `data/legalities.json`: LEGALITIES.
- Owner review:
  - #8675: `docs/decisions.md` entry, a station power multiplier does not
    raise a toughness stationer; `PC.tapPowerBonus` and
    `PC.tapsUsingToughness` folded into `PC.tapCounts`.
  - #8667: wire-format change, `AddPhases` became a record carrying its
    anchor (20 cards respelled).
  - #8671: wire-format change, `CantActivateAbilities` became `{"sources",
    "criteria"}` (Kang the Conqueror and Yuriko, Blade of the Mighty
    respelled).
  - #8679: wire-format change, the copy P/T exception widened to a defined
    P/T (13 cards respelled).
  - #8663: wire rename, `OpponentsAttacked` became
    `OpponentsAttackedThisCombat` (Dimir Strandcatcher respelled).
  - #8676: wire-format change, `RevealUntilChosenKind` gained a
    placement payload (Abundance respelled).
  - Additive wire shapes, no respelling: #8689 (`Keyword.Exert` takes an
    optional `Filter`), #8677 (`Modification.Goaded`, `Filter.IsGoaded`),
    #8692 (`Filter.IsPlayerInRole`), #8699 (a countered ability's
    last-known record in `GameState.snapshots`).
