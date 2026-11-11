# Secrets of Strixhaven census

The Secrets of Strixhaven (SOS) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-10 from MTGJSON's set list against `data/cards/`
(#6903). The set list is `data/decks/sets/sos/all.txt`, which the suite reads,
so every card of the set stays in the pool.

SOS has 271 cards (MTGJSON, bonus sheet included); 26 were already in the
pool, 245 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#6921) | 40 |
| Batches #6910 to #6919 | 190 |
| Converge batch #6920 (blocked by #6904) | 6 |
| Capability issues #6904 to #6909 | 8 |
| Behind an open issue elsewhere (#6252, made a sub-issue) | 1 |

- Trend: the first dry run translated 40 of the 245 missing; 15 needed a
  capability.

## Decks

None is committed: decks are optional, so `data/decks/sets/sos/` holds only
the set list. It writes each prepare card by MTGJSON's joined name
(`Emeritus of Conflict // Lightning Bolt`). Twelve of them name a prepare spell
after an older card, whose lookup answers that card; the finish taught
`Parun.DeckList.resolve` to take the card whose faces carry every half.

## How it went

Finished 2026-11-11.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 40 | #6921 |
| Batches | 195 | #6926 (19), #6927 (18), #6923 (19), #6928 (19), #6924 (19), #6930 (19), #6931 (19), #6933 (19), #6935 (19), #6936 (19), #6952 (6, Converge) |
| Behind a capability | 10 | below |

- Census trend: the first dry run translated 40 of the 245 missing; 15
  needed a capability. One verdict was overturned: Mind Roots (#6909) needed
  none, since the discard's slot already binds the cards (#6956).
- Capability issues, hardest first: #6904 Together as One, Arcane Omens and
  Rancorous Archaic (#6941), #6906 Zaffai and the Tempests (#6945), #6905
  Choreographed Sparks (#6944), #6909 Mind Roots (#6956), #6907 Improvisation
  Capstone (#6959), #6252 Moment of Reckoning (#6962), #6925 Burrog Barrage
  (#6963, dropped from batch 2), #6908 Mathemagics (#6964).
- Filed along the way: #6925 (a cast-this-turn count that leaves out the
  resolving spell), #6934 (Tablet of Discovery's mana yield in scenarios; a
  scenario mistake, #6940), #6939 (Tablet of Discovery under Rest in Peace,
  #6946), #6942 (an ability that can't be copied; open, Gogo, Master of
  Mimicry), #6947 (a departed spell's mana spent, #6954), #6948 (an attack
  trigger's "while" left unproven, #6961), #6958 (Bond of Agony's X under a
  free cast; outside the set, open).
- Audits: 5 of 8 capability PRs, 5 of 11 batch PRs and 4 of 8 fix PRs; 4
  found rules defects. Skipped the census, data-only batches and fixes, and
  the one-arm capability PRs #6956, #6963 and #6964, each recorded on #6902.
  - #6926: Ark of Hunger couldn't play a milled card a replacement exiled
    (#6929, fixed by #6937).
  - #6935: a cast trigger's "while" was read after state-based actions (#6938,
    fixed by #6943).
  - #6944: "can't be copied" and "can't be countered" were read off the
    printed card, missing copy effects (#6949, fixed by #6953).
  - #6945: Zaffai's free cast refused additional mana costs and skipped the X
    floor and mana-value prohibitions (#6951, fixed by #6955).
- Cost: not recorded (the units ran as cloud sessions whose transcripts the
  orchestrator could not reach). Wall clock ~5.5h, from the census PR's
  opening to the last capability PR's merge (#6962), then the finish.
- Owner review:
  - No `docs/decisions.md` entry. Moment of Reckoning's zero-mode cast (#6962)
    is forced by the Season cycle's ruling.
  - Codec keys added: #6927 (`CreateCopy.creator`), #6928
    (`CountersPutThisTurn.kind`, now optional), #6935 (`SpellCast.while`),
    #6944 (`Face.copyability`), #6945 (`CastFromZone.alternativeCost`), #6953
    (`counterability` and `copyability` on projected characteristics), #6954
    (`LastKnown.manaSpent`, `manaSpentOnX`), #6959
    (`TopOfLibraryUntil.weight`). Constructors added: #6937 (`GrantZone`),
    #6941 (`ManaSpent.Colors`), #6962
    (`ModeSelection.ChooseBetweenWithRepeats`), #6963
    (`CastWindow.OtherThanEvaluated`), #6964 (`Arithmetic.Exponentiation`).
