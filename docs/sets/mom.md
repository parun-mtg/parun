# March of the Machine census

March of the Machine (MOM) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117)
against `data/cards/` at 920d5794 (census #8596, tracker #8595). The set list
is `data/decks/sets/mom/all.txt`, which the suite reads, so every card of the
set stays in the pool. MOM was released 2023-04-21.

MOM has 296 card names, 357 faces (235 normal, 61 transform); 300 faces
were missing: 243 cards, 186 normal and 57 transform (33 of them Sieges).

| Place | Cards |
| --- | --- |
| Translated (census PR) | 29 |
| Batches #8600 through #8610 | 198 |
| Capability issues #8611 through #8622 | 16 |

- Trend: the first dry run translated 29 of the 243 missing; 16 needed a
  capability. `parun translate` skips double-faced cards, so the batches
  hand-wrote all 57.
- Blocked-by: no shape new to the pool was shared across batches (incubate,
  Sieges, backup and the Phyrexian-mana transform ability each had a pool
  card), so no batch blocked another. Shared edit sites, linked as blocked:
  `TriggerCondition` and `Parun.Engine.Event.Binding` (#8611 before #8612 and
  #8622); `Filter` (#8615, then #8616, then #8621).
- No keyword-only card was missing.

## Decks

None is committed: decks are optional, so `data/decks/sets/mom/` holds only
the set list, 296 names.

## How it went

Finished 2026-11-18.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 29 | #8624 |
| Batches | 194 | #8634 (19), #8642 (18), #8651 (20), #8654 (19), #8655 (14), #8660 (13), #8662 (15), #8659 (19), #8664 (20), #8665 (18), #8668 (19) |
| Behind a capability | 20, plus 1 from other sets | below |

- Census trend: the first dry run translated 29 of the 243 missing; 16
  needed a capability. Batches sent four more to capabilities they filed:
  Breach the Multiverse and Disturbing Conversion (batch 2, #8640, #8641),
  Khenra Spellspear (batch 7, #8656) and Urabrask (batch 11, #8661).
- Capability issues, hardest first: #8611 Zurgo and Ojutai (#8682), #8612
  Mirror-Shield Hoplite (#8693), #8613 Omnath, Locus of All (#8680), #8614
  Invasion of Fiora (#8685), #8615 Etched Host Doombringer, Joyful
  Stormsculptor, Portent Tracker and Rampaging Raptor (#8687), #8616 War
  Historian (#8695), #8617 Heliod, the Radiant Dawn (#8688), #8618 Inga and
  Esika (#8696), #8619 Angelic Intervention, with Giver of Runes from
  another set (#8694), #8620 Phyrexian Censor (#8691), #8621 Invasion of
  Pyrulea and Overgrown Pest (#8706), #8622 Norn's Inquisitor (#8697), then
  the batch-filed #8640 Breach the Multiverse (#8700), #8641 Disturbing
  Conversion (#8703), #8656 Khenra Spellspear (#8702; the reported defect did
  not reproduce, card data only), #8661 Urabrask (#8705).
- Fix issues: none.
- Filed along the way: #8658 (an Oculus Whelp condition mutation that stayed
  green; open, off the tracker), #8684 (I Bask in Your Silent Awe, the
  scheme reading #8614's window; open, under #8355).
- Audits: 1 of 16 capability PRs, by subagent: #8694 (new `Object` and
  LastKnown fields, a reshaped chosen quality); no fix issue followed.
  Reviewed inline: #8680, #8682 and #8691, clean. Skipped the census, the
  11 batches and the other 12 capability PRs (card data, lint-table
  entries, additive arms or parameters only the new card reads), each
  recorded on #8595.
- Cost: 4565 turns over 27 of the 28 unit PRs (#8680 gives no turn count),
  and over all 28: 618.7M cache read, 6.58M cache write, 2458k output, 829
  session-minutes. The finish unit's is on its own PR. Wall clock ~8.1h,
  from the tracker's opening (2026-11-18 05:16 UTC) to the last unit's
  merge (#8705, 13:24 UTC), then the finish.
- #8705 was dropped from the merge queue twice by a scenario expectation
  #8696 had made stale.
- `data/legalities.json`: the finish's `parun ingest` run, from MTGJSON of
  2026-11-17, added 3 rows and removed none: Invasion of Pyrulea, Overgrown
  Pest and Urabrask, merged after the TRC finish (#8708) regenerated the
  file. Every MOM name now has a row. It wrote no card; it restamped the
  out-of-set Blue Mage's Cane from a newer wording, reverted.
- Owner review:
  - #8685: `docs/decisions.md` entry extended, "What "since your last turn
    ended" spans before your first turn", to `WhichTurn`'s new
    since-your-last-turn window.
  - #8695: wire-format change, `DeclaredThisTurn`'s `"Attacker"` and
    `"Blocker"` became `{"type":"Attacked"}` and `{"type":"Blocked"}` (21
    cards respelled).
  - #8691: wire-format change, `CantCastMoreThan` became `{"count",
    "matching"}` (3 cards respelled).
  - #8696: creature-made mana carries a production tag, so ~215 scenarios
    restate it on their `ManaPool` checks and yields.
  - Additive wire shapes, no respelling: #8680 (`ColoredSymbol` takes an
    optional colour), #8682 (an `orBattle` flag on the damage-to-player
    trigger), #8685 (a `WhichTurn` window), #8687 (`Holder.Protector`),
    #8688 (a turn tally summed over players), #8693 (a backup qualifier on
    becomes-targeted), #8694 (`ChosenQuality.Colors` gains `colorless`),
    #8700 (a `Chooser` arm), #8705 (a permission's exile-instead rider),
    #8706 (`Filter.IsDoubleFaced`).
