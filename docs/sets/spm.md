# Marvel's Spider-Man census

The Marvel's Spider-Man (SPM) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-14 from MTGJSON's set list against `data/cards/`
(#7363). The set list is `data/decks/sets/spm/all.txt`, which the suite reads,
so every card of the set stays in the pool.

SPM has 193 cards and 198 face names (MTGJSON 5.3.0+20261113, against
`origin/main` at 4697ad42); 21 were already in the pool, 172 missing.
Layouts: 183 normal, 5 modal double-faced, 5 Sagas.

| Place | Cards |
| --- | --- |
| Added by the census (#7388) | 29 |
| Batches #7369 to #7375 | 134 |
| Capability issues #7378 to #7383, and the older #4489, #5514 and #6112 | 9 |

- Trend: the first dry run translated 29 of the 172 missing; 9 needed a
  capability.
- `parun translate` knew neither web-slinging nor mayhem, nor Food or
  Treasure tokens; the five double-faced cards and the five Sagas were absent
  from mtgish. The translator was not widened. No keyword-only SPM card was
  found to ingest.
- Batch order: batches 4 (#7372) and 5 (#7373) were blocked by batch 1
  (#7369), which first wrote "modified" (CR 700.9) as an `Or`.
- 38 batch cards carried a "verify" clause the census did not settle.

## Decks

MTGJSON lists five Welcome Decks (one a colour) and two Bundle Land Packs.
None is committed: decks are optional, so `data/decks/sets/spm/` holds only
the set list. It writes each of the five modal double-faced cards by
MTGJSON's joined name (`Peter Parker // Amazing Spider-Man`). SP//dr,
Piloted by Peni is one face whose name holds "//"; the finish taught
`Parun.DeckList.resolve` to look such a name up whole before splitting it.

## How it went

Finished 2026-11-14, against MTGJSON 5.3.0+20261113: the same 193 cards, every
face name in the pool.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 29 | #7388 |
| Batches | 128 | #7395 (18), #7396 (19), #7392 (18), #7400 (18), #7399 (18), #7391 (17), #7393 (20) |
| Behind a capability | 15, plus 10 from other sets | below |

- Census trend: the first dry run translated 29 of the 172 missing; 9 needed
  a capability. The batches overturned six verdicts, each card leaving its
  batch for an issue: Costume Closet, Maximum Carnage, Multiversal Passage,
  Scarlet Spider, Ben Reilly, Spider-Slayer, Hatred Honed and Spinneret and
  Spiderling.
- Capability issues, hardest first, then as filed: #7378 The Clone Saga
  (#7402), #5514 the greatest-power pools (#7405, adding Topple, Blot Out,
  Desecrator Hag and Draconic Fealty; Kraven split to #7401), #7379 Parker
  Luck (#7403), #7380 Spider-Verse (#7404), #6112 Behold the Sinister Six!
  (#7407), #4489 Norman Osborn // Green Goblin (#7409, adding Djinn
  Illuminatus, Herigast, Erupting Nullkite and Songcrafter Mage), #7381
  Spider-Man 2099 (#7411), #7382 Mister Negative (#7412), #7383 Arachne,
  Psionic Weaver (#7414); from the batches and capabilities, #7389 Spinneret
  and Spiderling (#7419, adding Deus of Calamity and Dragonborn Champion),
  #7390 Maximum Carnage (#7416), #7394 Costume Closet (#7424), #7397 Scarlet
  Spider, Ben Reilly (#7422), #7398 Multiversal Passage (#7420), #7401 Kraven
  the Hunter (#7421), and #5391 Spider-Slayer, Hatred Honed (#7426, adding
  Acidic Dagger), adopted from outside the set.
- Filed along the way: #7389, #7390, #7394, #7397, #7398 and #7401, by
  batches and capabilities, all closed. Off the tracker, each open: #7406
  (Long Rest and Blade Beam, FIN), #7408 (escape and miracle granted at a
  modified cost), #7418 ("is dealt N or more damage").
- Audits: 10 subagent audits, all of capability PRs (#7404, #7405, #7407,
  #7411, #7412, #7419, #7421, #7422, #7424, #7426); #7409, #7414 and the
  three fix PRs reviewed inline; the census, the seven batches and #7402,
  #7403, #7416 and #7420 skipped, each recorded on #7362. 3 found rules
  defects.
  - #7405: Topple's target pool reached outside its controller's range of
    influence (CR 801.11) (#7410, fixed by #7413).
  - #7412: a gain Tainted Remedy turned into a loss tallied as 0 for Mister
    Negative (#7415, fixed by #7417).
  - #7422: web-slinging's returned creature was offered as a source by the
    permanent's triggers, and a spell copy counted as cast using web-slinging
    (#7423, fixed by #7425).
- Cost: 4467 turns, 486.8M cache read, 6.87M cache write, 2357k output over
  the 27 unit PRs, each of which recorded one, 470 session-minutes in all.
  The finish unit's is on its own PR. Wall clock ~6h, from the tracker's
  opening to the last unit's merge (#7426), then the finish.
- Owner review:
  - #7421: `Operand.OfEachOtherCandidate` wire becomes `{measure, scope}` (28
    cards rewritten); the operand's shape was the implementer's call on the
    orchestrator's note to #7401, which the unit had flagged as an owner
    design call.
  - Wire changes, each with every pool card rewritten: #7404
    (`StateBasedActionDoesntApply` gains `matching`), #7407 (`Together` from
    a bare enum to tagged, 27 cards), #7411 (`LandsPlayedThisTurn` gains
    `excluding`), #7412 (`ExchangeLifeTotals` takes `{sides, tally}`), #7414
    (`ChosenQuality.CardType` takes an excluded list), #7419
    (`DamageTrigger.atLeast`), #7426 (`DamageRecipient.Permanent` gains
    `matching`, nine cards).
  - #7425: a spell copy no longer counts as cast using web-slinging (CR
    707.10), while sneak, mayhem and prowl's "cost was paid" readers keep
    copies; no ruling on copies for either, judged forced, no decisions.md
    entry.
  - #7424: a reading every last-known reader shares changed: it makes a
    departed Aura read as it last existed for every last-known reader, so
    Rhuk-style "equipped/enchanted" look-backs now see it (CR 603.10a).
