# Innistrad: Midnight Hunt census

The Innistrad: Midnight Hunt (MID) cards not yet in the pool, and which wait
on a capability. Derived 2026-11-21 from MTGJSON's set list (AllPrintings of
2026-10-26) against `data/cards/` at 0b3c53b7 (census #9197, tracker #9196).
The set list is `data/decks/sets/mid/all.txt`, which the suite reads, so every
card of the set stays in the pool.

MID has 321 face names, from 313 `normal` and 158 `transform` printings; the
set list holds its 272 card names, 49 of them transforming. 271 faces (225
cards) were missing.

| Place | Cards | Faces |
| --- | --- | --- |
| Translated (census PR) | 32 | 32 |
| Batches #9208 through #9216, and #9217 | 185 (43 transforming) | 228 |
| Capability issues #9201 through #9207 | 8 | 11 |

- Trend: the first dry run translated 32 of the 271 missing faces; 8 cards
  (11 faces, 7 issues) needed a capability, and 8 more waited on one of them
  (#9217, blocked by #9202).
- No MID card's whole text is keywords. Translate takes no transforming card
  (43 batch cards) and stopped at every planeswalker's loyalty field (Arlinn,
  the Pack's Hope; Wrenn and Seven), so those were hand-written.
- Shared shape new to the pool: "whenever day becomes night or night becomes
  day", with "if it's neither day nor night, it becomes day as this creature
  enters", on ten cards. #9202 landed it with Firmament Sage and Sunstreak
  Phoenix; #9217 held the other eight and was linked as blocked by #9202.
- No two capability issues shared an edit site the census could name; #9203
  and #9204 both concern Auras attached to players (an attach effect and a
  filter respectively).

## Decks

None is committed: decks are optional, so `data/decks/sets/mid/` holds only
the set list, 272 names.

## How it went

Finished 2026-11-21.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 32 | #9219 |
| Batches | 179 | #9225 (20), #9230 (18), #9231 (20), #9232 (20), #9236 (18), #9233 (20), #9239 (18), #9237 (19), #9242 (18), #9244 (8) |
| Behind a capability | 14, none from other sets | below |

- Census trend: the first dry run translated 32 of the 271 missing faces; 8
  cards needed a capability. Batches sent six more to capability issues:
  Corpse Cobble and Curse of Shaken Faith (batch 2, #9228 and #9229),
  Jack-o'-Lantern and Ludevic, Necrogenius (batch 5, #9234 and #9235), Search
  Party Captain (batch 7, #9238) and Vengeful Strangler (batch 9, #9240).
- Capability issues, hardest first: #9202 Firmament Sage and Sunstreak
  Phoenix (#9241), #9201 Enduring Angel (#9245), #9203 Curse of Leeches
  (#9246), #9206 Sigarda's Splendor (#9248), #9235 Ludevic, Necrogenius
  (#9258), #9240 Vengeful Strangler (#9261), #9207 Teferi, Who Slows the
  Sunset (#9263), #9205 Baneblade Scoundrel (#9268), #9204 Curse of
  Surveillance (#9290), #9228 Corpse Cobble (#9294), #9229 Curse of Shaken
  Faith (#9295), #9238 Search Party Captain (#9298), #9234 Jack-o'-Lantern
  (#9299).
- Fix issues, all from audits: #9247 (#9251), #9243 (#9250), #9252 (#9256),
  #9253 (#9257), #9259 (#9262), #9260 (#9264), #9265 (#9266), #9267 (#9269),
  #9255 (#9274), #9272 (#9275), #9254 (#9276), #9249 (#9283), #9278 (#9281),
  #9273 (#9282), #9284 (#9285), #9287 (#9289), #9300 (#9304).
- Filed along the way: #9228, #9229 by batch 2 (#9230), #9234, #9235 by batch
  5 (#9236), #9238 by batch 7 (#9239), #9240 by batch 9 (#9242), the fix
  issues above, and outside the tracker #9270 (Second Harvest) and #9271
  (Sutured Ghoul after devour), both from #9274, #9280 (Flameshadow
  Conjuring, from #9283), #9286 (shared team turns over-ask, from the #9282
  audit) and #9291 (shared team turns entry order, from the #9285 audit),
  none a MID card.
- Audits: 10 of 13 capability PRs (#9241, #9245, #9246, #9248, #9258, #9263,
  #9268, #9290, #9295, #9299), by subagent; 5 found rules defects. 15 of 17
  fix PRs; 8 found rules defects. Reviewed inline: batch #9230, capabilities
  #9261, #9294 and #9298, and fixes #9275 and #9289, clean. Skipped the census
  #9219 and batches #9225, #9231, #9232, #9233, #9236, #9237, #9239, #9242 and
  #9244 (card data and scenarios). Each is recorded on #9196.
  - #9245: simultaneous lethal hits each applied the replacement, and batched
    damage split into a loss then a gain (#9247).
  - #9246: the Curse was offered a player it cannot enchant (#9243).
  - #9248: the as-enters life note was read after a simultaneous entry's life
    payment, an unrecorded reading (#9252).
  - #9258: a transforms-into trigger sampled the permanent before its queued
    as-transforms copy (#9260).
  - #9295: a suspected double trigger on two free casts in one instruction,
    reproduced (#9300).
  - #9251: unreplaced batched damage still lost once per source, refuted by
    the Vampire Scrivener and Ob Nixilis, Captive Kingpin rulings (#9253).
  - #9256: seats kept the gathered order, so Ixidron beside another player's
    Clone depended on it (#9259).
  - #9262: a cross-seat order called forced that the rules leave open, and
    teammates seat-ordered under shared team turns (#9265).
  - #9264: a copied self-named "transforms into" trigger compared the literal
    name (#9267).
  - #9274: the entry gate ignored a row's own condition (#9278).
  - #9281: spurious entry-order prompts for conditioned rows (#9284).
  - #9283: a token copy of a merged permanent with a double-faced component
    on top became double-faced (#9287).
  - #9285: a shared-team-turns regression in the entry gate (#9291, outside
    the tracker).
- Cost: 693.0M cache read, 9.89M cache write, 3356k output and 6416 turns
  over all 41 unit PRs, 843 session-minutes in all. The finish unit's is on
  its own PR. Wall clock ~12.3h, from the tracker's opening (2026-11-21 02:14
  UTC) to the last unit's merge (#9304, 2026-11-21 14:32 UTC), then the
  finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-19. It gained 226 rows and lost none: 225 MID names the
  run added, and Curse of Clinging Webs (VOW, added by #9181 after the VOW
  finish). The other 47 MID names, reprints, already had rows. Every MID name
  now has a row. It wrote no card.
- Owner review:
  - `docs/decisions.md` entries: #9251 (rewritten by #9257: how simultaneous
    combat damage to a player loses life), #9266 ("Which seat's simultaneous
    as-enters effects apply first"), #9274 (the entry gate), #9276 ("How
    teammates' life floors share one damage event", "Whether a lifelink
    gain's replacement loss joins the damage's loss"), corrected by #9282.
  - Wire-format changes respelling cards: #9248 (`GameState.notedMana`
    became `notes`, `NoteManaSpent` became `Note NoteKind`; 1 card), #9250
    (`ChoosePlayer`; 1 card), #9263 (`UntapHold.DuringOthersSteps` takes a
    `PlayerRelation`; 5 cards), #9269 (`TransformTrigger`; 16
    cards), #9295 (`SpellCast` gains `Copies`; 3 cards).
  - Codec changes respelling no card: #9241 (`GameEvent.DaytimeChanged`),
    #9256 (a prompt ordering simultaneous entries' as-enters effects), #9258
    (a `Scratch` field), #9283 (`Source`, `TokenCopy` and `TokenLot` carry
    face data).
