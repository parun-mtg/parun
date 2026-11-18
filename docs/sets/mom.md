# March of the Machine census

March of the Machine (MOM) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-18 from MTGJSON's set list (5.3.0+20261117)
against `data/cards/` at 920d5794 (census #8596, tracker #8595). The set list
is `data/decks/sets/mom/all.txt`, which the suite reads, so every card of the
set stays in the pool. MOM was released 2023-04-21.

MOM has 296 card names and 357 face names: 235 normal and 61 transform (33 of
the transform cards are Sieges). 300 face names were missing: 243 cards, 186
normal and 57 transform.

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
  card), so no batch blocked another. Capabilities sharing an edit site were
  linked as blocked: #8612 and #8622 by #8611 (`TriggerCondition` and
  `Parun.Engine.Event.Binding`), #8616 by #8615 and #8621 by #8616
  (`Filter`).

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
  Breach the Multiverse and Disturbing Conversion (batch 2, #8640 and
  #8641), Khenra Spellspear (batch 7, #8656) and Urabrask (batch 11, #8661).
- Capability issues, hardest first: #8611 Zurgo and Ojutai (#8682), #8612
  Mirror-Shield Hoplite (#8693), #8613 Omnath, Locus of All (#8680), #8614
  Invasion of Fiora (#8685), #8615 Etched Host Doombringer, Joyful
  Stormsculptor, Portent Tracker and Rampaging Raptor (#8687), #8616 War
  Historian (#8695), #8617 Heliod, the Radiant Dawn (#8688), #8618 Inga and
  Esika (#8696), #8619 Angelic Intervention, with Giver of Runes from another
  set (#8694), #8620 Phyrexian Censor (#8691), #8621 Overgrown Pest and
  Invasion of Pyrulea (#8706), #8622 Norn's Inquisitor (#8697), then the
  batch-filed #8640 Breach the Multiverse (#8700), #8641 Disturbing
  Conversion (#8703), #8656 Khenra Spellspear (#8702; the reported defect
  did not reproduce, so card data only), #8661 Urabrask (#8705).
- Filed along the way: #8658 (a static ability's `HeldBy You` count
  condition left Oculus Whelp's scenario green under mutation; open, off the
  tracker), #8684 (I Bask in Your Silent Awe, the scheme reading spells cast
  over #8614's window; open, off the tracker).
- Audits: 1 of 16 capability PRs, #8694, by subagent; no fix issue was
  filed, and its findings are not recorded. Reviewed inline: #8680, #8682
  and #8691. Skipped the census, the 11 batches and the other 12 capability
  PRs (card data, renderer wording, lint-table entries, or small additive
  shapes only the new card reads), each recorded on #8595.
- Cost: 618.7M cache read, 6.58M cache write, 2458k output over all 28 unit
  PRs, and 4565 turns over 27 of them (#8680 records no turn count); 829
  session-minutes in all. The finish unit's is on its own PR. Wall clock
  ~8.1h, from the tracker's opening (2026-11-18 05:16 UTC) to the last
  unit's merge (#8705, 13:24 UTC), then the finish.
- Owner review:
  - #8685: `docs/decisions.md`, the entry "What "since your last turn ended"
    spans before your first turn" gained a second site,
    `Parun.Engine.Game.turnLog`'s `SinceOwnLastTurn`.
  - #8695: wire-format change, `DeclaredThisTurn`'s role became a tagged
    `DeclaredRole` (`"Attacker"` to `{"type":"Attacked"}`, `"Blocker"` to
    `{"type":"Blocked"}`; 21 cards respelled).
  - #8691: wire-format change, `CantCastMoreThan` became `{"count",
    "matching"}` (3 cards respelled).
  - Additive wire shapes, no respelling: #8680 (`ColoredSymbol` takes an
    optional colour), #8682 (an `orBattle` flag on the damage-to-player
    trigger), #8694 (`ChosenQuality.Colors` gains a defaulted `colorless`
    key, and `Object` a `chosenColorless` field), #8700
    (`Chooser.TheControllerInEach`), #8705 (`CastFromZone` gains
    `exiledInstead`), #8706 (`Filter.IsDoubleFaced`).
