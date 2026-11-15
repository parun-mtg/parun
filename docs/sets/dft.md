# Aetherdrift census

The Aetherdrift (DFT) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-15 from MTGJSON's set list against `data/cards/`
(#7745). The set list is `data/decks/sets/dft/all.txt`, which the suite reads,
so every card of the set stays in the pool.

DFT has 276 card names (MTGJSON 5.3.0+20261114, against `origin/main` on
2026-11-15), every printing `layout: normal`; 44 were already in the pool, 232
missing.

| Place | Cards |
| --- | --- |
| Added by the census (#7768) | 16 |
| Batches #7756 to #7765 | 195 |
| Capability issues #7747 to #7754 | 21 |

- Trend: the first dry run translated 16 of the 232 missing; 21 needed a
  capability.
- Batches 4, 5, 6 and 8 were blocked by batch 1 (#7756), which wrote "becomes
  saddled" and an exhaust cost with {X} first; batches 5 and 8 also by batch 3
  (#7758), which wrote "Enchant creature or Vehicle" first.
- #7749 (crew using toughness) and #7751 (cycling with {X}) were blocked by
  #7747 (the Pilot crew bonus), which 12 of the 21 capability cards waited on.
- No keyword-only DFT card was found to ingest. The translator was not
  widened.

## Decks

None is committed: decks are optional, so `data/decks/sets/dft/` holds only
the set list. The set has no multi-faced card, so every line is a plain card
name.

## How it went

Finished 2026-11-15, against MTGJSON 5.3.0+20261114: the same 276 cards,
every face name in the pool.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 16 | #7768 |
| Batches | 193 | #7778 (19), #7781 (20), #7779 (18), #7789 (20), #7791 (19), #7788 (20), #7780 (18), #7793 (20), #7774 (19), #7785 (20) |
| Behind a capability | 23, plus 1 from other sets | below |

- Census trend: the first dry run translated 16 of the 232 missing; 21
  needed a capability. The batches overturned two verdicts, each card leaving
  its batch for an issue: Dune Drifter (#7772, from batch 3) and Radiant Lotus
  (#7777, from batch 7).
- Capability issues, hardest first, then as filed: #7747 the Pilot crew bonus,
  twelve cards (#7804); #7748 Gonti, Night Minister (#7802); #7749 Interface
  Ace (#7816); #7750 The Aetherspark (#7799); #7751 Valor's Flagship and
  Webstrike Elite (#7821); #7752 Canyon Vaulter and Reckless Velocitaur
  (#7829); #7753 Basri, Tomorrow's Champion (#7810); #7754 Outpace Oblivion
  (#7815). From the batches: #7772 Dune Drifter (#7812); #7775 Fearless
  Swashbuckler, already in the pool, its "attacked this combat" rewritten
  (#7798); #7777 Radiant Lotus (#7814); #7790 Lightwheel Enhancements, already
  in the pool, switched to a bare graveyard cast (#7800, adding Skaab
  Ruinator).
- Filed along the way: #7772, #7775 and #7790 by batch 3 and batch 5, #7777
  by batch 7, all closed. Off the tracker, open: #7811 (the text client hides
  the power a creature crews or saddles for).
- Audits: 8 subagent audits of the 12 capability PRs (#7798, #7799, #7800,
  #7802, #7804, #7814, #7816, #7821); #7810, #7812, #7815 and #7829 reviewed
  inline; the census and the ten batches skipped, each recorded on #7744. None
  found a rules defect. Their test gaps and a sibling were folded into the
  finish unit: an opponent's-turn scenario for The Aetherspark's
  intervening "if", a parameter on `Projection.projectWithLastKnown` for
  `Mana.productionTagsGiven`, and a stale scenario name in a haddock.
- Cost: 428.7M cache read, 5.27M cache write, 1832k output over the 23 unit
  PRs, each of which recorded one (turns not recorded by #7774 and #7785; 3290
  over the other 21), 508 session-minutes in all. The finish unit's is on its
  own PR. Wall clock ~5.0h, from the census issue's labelling (09:48 UTC) to
  the last unit's merge (#7829, 14:45 UTC), then the finish.
- Owner review:
  - #7804: decisions.md "Whether two crew power offsets add" (offsets add).
    Only an Alchemy card (Stoic Star-Captain, YEOE) observes it, and its
    audit argued CR 613.11 may force the answer, which would make it a site
    citation instead.
  - #7816: decisions.md "Whether a crew power offset raises a toughness
    crewer" (a 0/4 Interface Ace counts 4). Again only Stoic Star-Captain
    observes it; its audit called the case against timestamp order circular,
    the decision itself sound.
  - Wire changes: #7804 and #7816 (`Modification.TapsCounting` with
    `TapCount`, and `TapForTotalPower.keywordCost`); #7798
    (`EventShape.AttackerDeclared` gains an `AttackWindow`); #7829
    (`CrewRole.Crewer` gains a keyword family, every pool Crewer rewritten).
