# Edge of Eternities census

The Edge of Eternities (EOE) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-14 from MTGJSON's set list against `data/cards/`
(#7465). The set list is `data/decks/sets/eoe/all.txt`, which the suite reads,
so every card of the set stays in the pool.

EOE has 266 card names (MTGJSON 5.3.0+20261113, against `origin/main` at
bc88f282), every card `layout: normal` (32 Spacecraft, 15 Planets); 19 were
already in the pool, 247 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#7486) | 32 |
| Batches #7473 to #7483 | 210 |
| Capability issues #7468 to #7472 | 5 |

- Trend: the first dry run translated 32 of the 247 missing; 5 needed a
  capability.
- `parun translate` knew neither station, warp nor the Lander token
  (`_Rule:StationCharged`, `_Rule:Warp` and `_CreatableToken:LanderToken` led
  its rejections). The translator was not widened. No keyword-only EOE card
  was found to ingest.
- Batch order: batches 2 to 11 were blocked by batch 1 (#7473), which first
  wrote the Lander token, the 2/2 colorless Robot token and station on a
  Planet; batches 3, 7, 9 and 10 also by batch 2 (#7474), which first wrote a
  station symbol's triggered ability and the flying Drone token.
- 41 batch cards carried a "verify" clause the census did not settle.

## Decks

MTGJSON lists a Bundle Land Pack and an MTGO Redemption deck. Neither is
committed: decks are optional, so `data/decks/sets/eoe/` holds only the set
list. The set has no multi-faced card, so every line is a plain card name.

## How it went

Finished 2026-11-14, against MTGJSON 5.3.0+20261114: the same 266 cards,
every face name in the pool.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 32 | #7486 |
| Batches | 207 | #7487 (20), #7496 (19), #7514 (18), #7494 (19), #7498 (19), #7497 (19), #7507 (19), #7495 (19), #7509 (19), #7515 (17), #7500 (19) |
| Behind a capability | 8, plus 1 from other sets | below |

- Census trend: the first dry run translated 32 of the 247 missing; 5 needed
  a capability. The batches overturned three verdicts, each card leaving its
  batch for an issue: Dyadrine, Synthesis Amalgam, Tannuk, Steadfast Second
  and The Dominion Bracelet.
- Capability issues, hardest first, then as filed: #7468 Weftwalking
  (#7533), #7469 Close Encounter (#7522), #7470 Quantum Riddler (#7530),
  #7471 Timeline Culler (#7528), #7472 Uthros Psionicist (#7524, adding
  Highspire Bell-Ringer); from the batches, #7508 Tannuk, Steadfast Second
  (#7526), #7512 Dyadrine, Synthesis Amalgam (#7523) and #7513 The Dominion
  Bracelet (#7532).
- Filed along the way: #7508, #7512 and #7513, by batches, all closed. Off
  the tracker, open: #7499 (Alternating Teams "not your turn", Zealous
  Display).
- Audits: 5 subagent audits, of capability and fix PRs (#7522, #7524, #7526,
  #7530, #7539); #7528, #7531, #7532 and #7533 reviewed inline; the census,
  the eleven batches and #7523 skipped, each recorded on #7464. 2 found rules
  defects.
  - #7522: Close Encounter's behold reaching exile still excused a "cast it
    if able" (CR 118.8c) (#7529, fixed by #7531).
  - #7530: the draws Lich and Chains of Mephistopheles make in another
    event's place skipped the draw instruction, so Quantum Riddler and Alms
    Collector missed them (#7536, fixed by #7539).
- Cost: 3167 turns, 381.4M cache read, 4.87M cache write, 1803k output over
  the 22 unit PRs, each of which recorded one, 454 session-minutes in all.
  The finish unit's is on its own PR. Wall clock ~5.7h, from the census
  issue's labelling (about 13:40 UTC) to the last unit's merge (#7539, 19:20
  UTC), then the finish.
- Owner review:
  - #7526: decisions.md "Whether a warp granted to a card in a hand carries
    onto the spell" (yes, by CR 400.7g; no ruling on Tannuk, Steadfast
    Second).
  - #7530 and #7539: every draw (the draw step's, each spell's, learn's, and
    those Lich, Chains of Mephistopheles, Alhammarret's Archive, Teferi's
    Ageless Insight and Aladdin's Lamp make) now goes through one CR 121.2a
    draw instruction, and the rows applied to it carry to its individual
    draws (CR 614.5).
  - Wire changes, each with every pool card rewritten: #7522 (`Behold`
    gains `cardZone`, defaulted to the hand), #7524
    (`CostModifier.onlyFirst` becomes `onlyNth`, a new `TurnOrdinal`, five
    cards), #7533 (`FreeCast` gains `onlyNth`).
  - #7524: `Parun.Types.TurnOrdinal` (scope plus ordinal) now sits beside
    `Parun.Types.SpellCast`'s own scope and ordinal fields: a consolidation
    candidate.
  - #7499: moved off the tracker as a variant corner, not set work.
