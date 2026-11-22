# Adventures in the Forgotten Realms census

The Adventures in the Forgotten Realms (AFR) cards not yet in the pool, and
which wait on a capability. Derived 2026-11-21 from MTGJSON's set list
(AllPrintings of 2026-11-19) against `data/cards/` at 84f0f971 (census #9313,
tracker #9312). The set list is `data/decks/sets/afr/all.txt`, which the suite
reads, so every card of the set stays in the pool.

AFR has 266 face names, from 390 `normal` and 12 `class` printings, every card
single-faced; the set list holds its 266 card names. 228 were missing.

| Place | Cards |
| --- | --- |
| Translated (census PR) | 35 |
| Batches #9324 through #9332 | 186 (10 Classes, 5 planeswalkers) |
| Capability issues #9317 through #9323 | 7 |

- Trend: the first dry run translated 35 of the 228 missing; 7 cards (7
  issues) needed a capability.
- No AFR card's whole text is keywords. Translate takes no `class` layout card
  and stopped at every planeswalker's loyalty field, so those were
  hand-written.
- No shape new to the pool spanned two batches, so no batch blocked another.
  The Book of Vile Darkness (#9318) was blocked by Hand of Vecna (#9323),
  whose card its cost exiles. Critical Hit (#9321) and Farideh, Devil's
  Chosen (#9320) both extend the die-roll trigger.

## Decks

None is committed: decks are optional, so `data/decks/sets/afr/` holds only
the set list, 266 names.

## How it went

Finished 2026-11-22.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 35 | #9333 |
| Batches | 181 | #9355 (21), #9356 (19), #9358 (21), #9363 (20), #9362 (20), #9364 (20), #9369 (20), #9367 (19), #9370 (21) |
| Behind a capability | 12, none from other sets | below |

- Census trend: the first dry run translated 35 of the 228 missing; 7 cards
  needed a capability. Batches sent five more to capability issues: Dragon's
  Disciple (batch 2, #9354), Inferno of the Star Mounts (batch 4, #9360),
  Mordenkainen (batch 6, #9361), Split the Party (batch 7, #9368) and Volo,
  Guide to Monsters (batch 8, #9366).
- Capability issues, hardest first: #9317 Hama Pashar, Ruin Seeker (#9372),
  #9320 Farideh, Devil's Chosen (#9373), #9360 Inferno of the Star Mounts
  (#9371), #9323 Hand of Vecna (#9374), #9318 The Book of Vile Darkness
  (#9379), #9322 Keen-Eared Sentry (#9380), #9366 Volo, Guide to Monsters
  (#9390), #9321 Critical Hit (#9405), #9319 Wish (#9408), #9354 Dragon's
  Disciple (#9411), #9361 Mordenkainen (#9415), #9368 Split the Party
  (#9419).
- Fix issues: #9375 (#9376), #9377 (#9378), #9381 (#9383), #9385 (#9387),
  #9386 (#9398), #9389 (#9396), #9400 (#9403), #9404 (#9406), #9407 (#9420),
  #9410 (#9414), #9393 (#9427), #9394 (#9441), #9426 (#9446), #9433 (#9451).
  #9392, #9395 and #9397 were duplicate splits of #9389, closed unworked;
  #9401, a second session's PR for #9389, closed unmerged.
- Filed along the way: the five capability issues above, by their batches;
  #9375, #9377, #9381, #9385, #9389, #9400, #9404, #9407, #9410 and #9433 by
  audits; #9386 by #9387; #9393 and #9394 split from #9389 by #9396; #9426 by
  #9427. Outside the tracker, none needing an AFR card and none counted here:
  #9365 (subtype-setting modes, from #9362), #9402 (a roll's trigger checked
  after its payments, from the #9396 and #9414 audits), #9412 (sideboard
  size in object ids, from the #9408 audit), #9423 (Night Shift's reroll
  reading, from the #9414 audit), #9436 and #9437 (tied reroll groups, coin
  reflips, from #9441), #9445 (Monitor Monitor, from #9446) and #9450 (three
  Walls, from #9451).
- Audits: 8 of 12 capability PRs (#9372, #9373, #9379, #9380, #9390, #9405,
  #9408, #9411), by subagent; 4 found rules defects, and #9405 a contested
  reading. 8 of 14 fix PRs (#9378, #9383, #9396, #9398, #9403, #9414, #9427,
  #9441); #9398 introduced defects, #9378 and #9383 found older ones beside
  them, #9427 display defects, and #9396 and #9414 corners only non-AFR cards
  reach. Reviewed inline: capabilities #9374, #9415 and #9419, and fixes
  #9376, #9387, #9406, #9420, #9446 and #9451. Skipped the census #9333,
  every batch and #9371 (card data and scenarios). Each is recorded on #9312.
  Chains: die rolls #9373, #9378, #9383, #9396, #9405, #9414, #9427, #9441,
  ended at #9446 once findings needed only non-AFR dice cards; The Book of
  Vile Darkness #9379, #9387, #9398, #9406; Volo #9390, #9403; Hama Pashar
  #9372, #9376.
  - #9372: Fractured Realm's empty bearer filter doubled a dungeon's room
    abilities (#9375).
  - #9373: a simultaneous reroll of several dice logged one roll per die, and
    a reroll's number counted rerolls nested inside it (#9377).
  - #9379: an exiled card's triggered keywords (prowess) never reached Vecna
    (#9385).
  - #9390: the spell filter's pools were read after state-based actions, not
    at the cast (#9400).
  - #9405: its reading that a natural 20 rerolled away does not trigger was
    contestable and conflicted with the Farideh entry (#9410, which reversed
    it).
  - #9378: a simultaneous reroll was still several rolls to replacements, and
    a Die creature's reroll recorded its final result (#9381).
  - #9383: a reroll's event survived the lowest-roll ignore (#9389).
  - #9398: mutate, text-box exchange and text changes dropped Vecna's lent
    keywords (#9404).
  - #9427: the web client dropped the offer's die, and the prompt could ask
    among identical options (#9433).
- Cost: 632.8M cache read, 9.29M cache write, 3109k output and 5535 turns
  over all 36 unit PRs, 798 session-minutes in all. The finish unit's is on
  its own PR. Wall clock ~13.7h, from the tracker's opening (2026-11-21 15:58
  UTC) to the last unit's merge (#9451, 2026-11-22 05:38 UTC), then the
  finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-19. It gained 232 rows and lost none: 228 AFR names the
  run added, and Loch Larent (#9336), Second Harvest (#9307), Underbridge
  Warlock (#9384) and Molten Impact (#9391), none AFR, added after the MID
  finish regenerated the file. The other 38 AFR names, reprints, already had
  rows. Every AFR name now has a row. It wrote no card.
- `data/printings.json`: not regenerated, though #9408 (Wish) deferred it
  here: the set list names cards without `(AFR) NUMBER` suffixes, as every
  set list does, so nothing reads AFR's printings.
- Owner review:
  - `docs/decisions.md` entries: "Which result a reroll's trigger reads"
    (#9373, rewritten in #9378); "Whether a reroll reaches a die before the
    lowest roll is ignored" (extended in #9396); "Which ventures Keen-Eared
    Sentry counts" (#9380); "Whether a natural 20 rerolled away was rolled"
    (#9405, reversed in #9414, its rejected reading reworded in #9441).
  - Wire-format changes: `DieResult` and `RollTrigger` (#9405);
    `DieResult.reading` (#9414); `Prompt.ChooseRollModifier`'s per-candidate
    payload (#9427, #9441, #9451); `Prompt.Rethrow` and `ChooseDieResult`
    showing every die (#9441); the `CastFromZone` (#9408) and `OrTapped`
    (#9411) codec reshapes.
