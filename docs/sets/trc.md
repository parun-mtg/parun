# Star Trek Commander census

Star Trek Commander (TRC) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117)
against `data/cards/` at 2e59028b, in one census with Stardates
(`docs/sets/sds.md`; the two sets share no card name). The set list is
`data/decks/sets/trc/all.txt`, which the suite reads, so every card of the set
stays in the pool. TRC was released 2026-11-13.

TRC has 336 card names, all single-faced (332 normal, 4 saga); 244 were
missing.

| Place | TRC | With SDS |
| --- | --- | --- |
| Added by the census (#8547) | 45 | 48 |
| Batches #8525 through #8534 | 180 | 198 |
| Batch #8546 (Battlefront, blocked by #8542) | 6 | 6 |
| Capability issues #8535 through #8545 | 13 | 13 |

- Trend: the first dry run translated 48 of the 265 missing across both
  sets; 19 needed a capability (13 in the capability issues, 6 in #8546).
- Blocked-by: shapes new to the pool were each first written by one batch
  (typed cycling duals and Parley by #8525, reveal-an-X-or-Y lands by #8526,
  the Klingon-or-Warrior pair by #8528, "enters tapped unless two or more
  opponents" by #8530); the batches reusing them were linked as blocked by
  those. #8541 waited on #8537 (both extend `Parun.Types.Filter`) and #8543
  on #8542 (both extend `Parun.Types.Quantity`).
- No keyword-only card was missing (`parun ingest` wrote none).

## Decks

None is committed: decks are optional, so `data/decks/sets/trc/` holds only
the set list, 336 names.

## How it went

Finished 2026-11-18. The figures cover both sets, which ran under one
tracker (#8510); `docs/sets/sds.md` gives SDS's share.

| Route | TRC | SDS | PRs |
| --- | --- | --- | --- |
| Translated (census) | 45 | 3 | #8547 |
| Batches | 183 | 17 | #8566 (20), #8589 (19), #8583 (19), #8631 (20), #8648 (20), #8630 (20), #8632 (20), #8645 (19), #8647 (19), #8653 (18), #8674 (6) |
| Behind a capability | 16 | 1 | below |

- Census trend: the first dry run translated 48 of the 265 missing; 19
  needed a capability. Batches sent five more to capabilities they filed:
  Combat Celebrant (batch 2, #8575), Command Chair (batch 2's
  commander-spell reading, #8579, closed not planned: rulings confine
  "commanders you control" to the battlefield, and batch 2 wrote it so),
  Ertai Resurrected (batch 3, #8580, the one SDS card), Shared Animosity
  (batch 8, #8643) and The Guardian of Forever (batch 9, #8637).
- Capability issues, hardest first: #8535 World at War (#8667), #8536
  Return to Dust (#8666), #8537 Kang, Suspicious Captain and Koloth, Tribble
  Hunter (#8677), #8538 Sulu, Heroic Helmsman (#8675), #8539 Colossal Clone
  Master (#8679), #8540 Chief Montgomery Scott (#8672), #8541 Cmdr. Troi,
  Outgoing Empath (#8692), #8542 Korok, Liberated Rage and Kurn, Son of Mogh
  (#8663), #8543 Guinan, Ancient Advisor (#8673), #8544 The Immortal Sun
  (#8671), #8545 Pavel Chekov, First Posting (#8676); then the batches':
  #8575 Combat Celebrant (#8689), #8580 Ertai Resurrected (#8699), #8637
  The Guardian of Forever (#8698), #8643 Shared Animosity (#8704, lint-table
  entries only).
- Fix issues: #8678 World at War's trigger fired at a later combat when its
  added combat was skipped (#8681), #8683 Sulu tripled his power stationing
  a Planet too (#8686), #8690 two surviving mutations in Kang's goad,
  scenarios only (#8701).
- Filed along the way, off the tracker as not TRC or SDS card work, all
  open: #8576 (a Spacecraft's last station striation ungated), #8669 (a
  departed creature's last-known goaded designation), #8670 (a goader fixed
  at resolution for "goaded for the rest of the game").
- Audits: 3 of 18 capability, fix or test PRs, by subagent; 2 found rules
  defects. Reviewed inline: #8666, #8671, #8672, #8679 and #8689, clean.
  Skipped the census, the 11 batches and #8663, #8673, #8676, #8681, #8686,
  #8692, #8698, #8699, #8701 and #8704 (card data, renderer wording,
  lint-table entries, additive arms only the new card reads, or an audited
  finding fixed in place), each recorded on #8510.
  - #8667: World at War's "that combat" trigger outlived its skipped
    combat, fix #8678.
  - #8675: the tripling applied to stationing Planets, not only
    Spacecraft, fix #8683.
  - #8677: no rules defect; two surviving mutations, proved by #8690.
- Cost: 3634 turns, 459.5M cache read, 5.4M cache write, 1914k output over
  29 of the 30 unit PRs; #8675 stated none. 696 session-minutes in those 29.
  The finish unit's is on its own PR. Wall clock ~9.7h, from the tracker's
  opening (2026-11-18 01:54 UTC) to the last capability PR's merge (#8704,
  11:35 UTC), then the finish.
- `data/legalities.json`: 425 rows added, 178 of them TRC or SDS cards (the
  rest landed alongside by other sets' units); every TRC and SDS card now
  has a row. The finish's `parun ingest` run, from MTGJSON of 2026-11-17,
  wrote no card and restamped the out-of-set Blue Mage's Cane from a newer
  wording, reverted.
- Owner review:
  - #8675: `docs/decisions.md` entry, a station power multiplier does not
    raise a toughness stationer; `PC.tapPowerBonus` and
    `PC.tapsUsingToughness` folded into `PC.tapCounts`.
  - #8667: wire-format change, `Effect.AddPhases` became a record with an
    anchor (20 cards respelled).
  - #8671: wire-format change, `CantActivateAbilities` became `{"sources",
    "criteria"}` (Kang the Conqueror and Yuriko, Blade of the Mighty
    respelled).
  - #8679: wire-format change, the copy P/T exception gained a kind (13
    cards respelled).
  - #8686: wire-format change, the tap-counting grant gained a Filter (15
    cards respelled).
  - #8663: wire-format change, `OpponentsAttacked` renamed
    `OpponentsAttackedThisCombat` (Dimir Strandcatcher respelled).
  - #8676: the reveal-until draw rewrite gained an order (Abundance
    respelled).
  - Additive wire shapes, no respelling: #8672 (an entrant gate on an entry
    rider's counters), #8689 (an optional Filter on `Exert`), #8692 (a
    turn-player Filter atom), #8699 (a countered ability's controller kept
    in `GameState.snapshots`).
