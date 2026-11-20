# Kamigawa: Neon Dynasty census

The Kamigawa: Neon Dynasty (NEO) cards not yet in the pool, and which wait on
a capability. Derived 2026-11-20 from MTGJSON's set list (AllPrintings of
2026-11-17) against `data/cards/` at 4cb36b4b (census #9024, tracker #9023).
The set list is `data/decks/sets/neo/all.txt`, which the suite reads, so every
card of the set stays in the pool.

NEO has 310 face names, from 473 `normal` and 84 `transform` printings; the
set list holds its 287 card names, 23 of them transforming. 258 faces were
missing.

| Place | Cards | Faces |
| --- | --- | --- |
| Translated (census PR) | 19 | 19 |
| Batches #9031 through #9041 | 211 (21 transforming) | 232 |
| Capability issues #9026 through #9030 | 6 | 7 |

- Trend: the first dry run translated 19 of the 258 missing faces; 6 cards (7
  faces, 5 issues) needed a capability.
- No NEO card's whole text is keywords. Translate takes no transform card (the
  21 transforming Sagas and creatures) and rejects planeswalkers (#5711:
  Kaito Shizuki, Tezzeret, Betrayer of Flesh), so those were hand-written.
- Shared shape new to the pool: the channel lands' "costs {1} less to activate
  for each legendary creature you control", written first by #9032 (Boseiju,
  Who Endures); #9033, #9038, #9039 and #9040 were linked as blocked by it.
  #9030 was linked as blocked by #9029 (both extend counter placement).

## Decks

None is committed: decks are optional, so `data/decks/sets/neo/` holds only
the set list, 287 names.

## How it went

Finished 2026-11-20.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 19 | #9042 |
| Batches | 205 | #9046 (19), #9044 (18), #9063 (19), #9045 (19), #9057 (19), #9062 (18), #9073 (19), #9072 (19), #9076 (20), #9080 (16), #9081 (19) |
| Behind a capability | 12, plus 1 from other sets | below |

- Census trend: the first dry run translated 19 of the 258 missing faces; 6
  cards needed a capability. Batches sent six more cards to issues of their
  own: Boseiju, Who Endures (#9043, batch 2: a from-hand cost reduction
  seemed to read 0, which did not reproduce), Inventive Iteration // Living
  Breakthrough (#9054, batch 5: a prohibition's mana-value comparison was not
  baked), Lethal Exploit (#9060, batch 6: an as-cast read inside a
  modification gave 0), and from batch 10 Storyweave (#9077: a once-only entry
  replacement gave nothing to simultaneous entries), The Dragon-Kami Reborn //
  Dragon-Kami's Egg (#9078: a face-down exiled card's types were not visible
  to the cast offer) and The Kami War // O-Kagachi Made Manifest (#9079: no
  chooser picked in another player's graveyard).
- Capability issues, hardest first: #9078 The Dragon-Kami Reborn (#9084, the
  engine diff a comment only), #9077 Storyweave (#9085), #9026 Crackling and
  Harmonious Emergence (#9087), #9027 Hidetsugu Consumes All (#9088), #9029
  Invoke the Ancients (#9090), #9030 Dramatist's Puppet (#9094, which also
  added Quarry Hauler, of Kaladesh, sharing the wording), #9028 Invoke
  Calamity (#9093), #9054 Inventive Iteration (#9100), #9060 Lethal Exploit
  (#9101), #9079 The Kami War (#9103). #9043 Boseiju was labelled ahead of
  the batches four of them waited on, and landed as card data only (#9049).
- Filed along the way: #9043 by batch 2 (#9044), #9054 by batch 5 (#9057),
  #9060 by batch 6 (#9062), #9077, #9078 and #9079 by batch 10 (#9080), #9048
  from batch 4's second renderer path (#9045), #9083 by #9084, #9086 by the
  #9026 unit while in flight, #9096 by the audit of #9092, #9097 by #9098, and
  #9106 by the audit of #9102.
- Fix issues: #9048 one renderer path for "as long as enchanted permanent is
  a creature" (#9082); #9083 an offer stating a quality could cast a
  face-down exiled card its caster may not look at, with a synthetic (#9104);
  then a chain on destruction replacements and the state-based check, each
  found by auditing the previous fix: #9086 a destruction's instead-effects
  ran after its event, missing look-back triggers (#9092); #9096 Lich's
  Mirror's reset and Crackling Emergence's sacrifice both moved the Aura in
  one check (#9098); #9097 the reset still double-moved an Aura that umbra
  armor or a prompting replacement moved (#9102); #9106 queued instead-effects
  moved on the live board, missing Rest in Peace (#9108).
- Audits: 2 of 11 capability PRs (#9085, #9087), plus the fixes #9092, #9098,
  #9102 and #9108, by subagent; 4 found rules defects, one of them partly
  pre-existing. Reviewed inline: batches #9044, #9046, #9045, #9062, #9076
  and #9081, capabilities #9084, #9088, #9090, #9093, #9094, #9100, #9101 and
  #9103, and fixes #9082 and #9104, clean. Skipped the census, the other five
  batches and #9049 (card data and scenarios). Each is recorded on #9023.
  - #9085: clean.
  - #9087: the instead-effects ran after the destruction event, so a
    look-back trigger of a permanent leaving alongside the Aura missed
    (Mayhem Devil against Wrath of God); filed #9086.
  - #9092: with Lich's Mirror, the Emergence sacrifice and the reset both
    moved the Aura in one state-based pass; filed #9096.
  - #9098: umbra armor's Aura (pre-existing) and an Emergence sacrifice that
    prompts still escaped the contest with the reset; #9097 rescoped to both.
  - #9102: queued Aura moves ran on the live board, so a Rest in Peace
    leaving in the same event no longer exiled the Aura; filed #9106.
  - #9108: clean.
- Cost: 546.2M cache read, 5.94M cache write, 2207k output and 4153 turns
  over all 29 unit PRs, 630 session-minutes in all. The finish unit's is on
  its own PR. Wall clock ~9.1h, from the tracker's opening (2026-11-20 09:17
  UTC) to the last unit's merge (#9108, 2026-11-20 18:25 UTC), then the
  finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-19. It gained 237 rows and lost none: the 236 NEO names
  the run added, and Quarry Hauler. The other 51 NEO names, reprints, already
  had rows. Every NEO name now has a row. It wrote no card.
- Owner review: no `docs/decisions.md` entry this run.
  - Wire-format changes respelling cards: #9093 (`CastRepetition.UpTo`
    became `{casts, total?}`; Baron Helmut Zemo, Doom Reigns Supreme, Plargg
    and Nassari, Talent of the Telepath respelled), #9094
    (`AnotherCounter.EachKind` carries a `KindStep`; Powerful Broker
    respelled), #9103 (`Chooser.BoundInSlot` chooses across its scope;
    Skullwinder's scope respelled).
  - Codec changes respelling no card: #9085 (`Scratch.spentEntries`), #9087
    (`DestructionR` parametric, `DestructionRewrite.RunEffects`), #9088
    (`ObjectDamage.to`), #9090 (`PutCounters.alternatives`), #9108
    (`ReplacedDestruction` in `Snapshots`, `DestructionCause`).
