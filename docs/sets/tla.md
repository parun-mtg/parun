# Avatar: The Last Airbender census

The Avatar: The Last Airbender (TLA) cards not yet in the pool, and which
wait on a capability. Derived 2026-11-13 from MTGJSON's set list against
`data/cards/` (#7225). The set list is `data/decks/sets/tla/all.txt`, which
the suite reads, so every card of the set stays in the pool.

TLA has 286 cards and 294 face names (MTGJSON 5.3.0+20261112, against
`origin/main` at 61cf266a); 37 were already in the pool, 249 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#7245) | 45 |
| Batches #7235 to #7244 | 192 |
| Capability issues #7226 to #7234, and the older #4442 and #4529 | 12 |

- Trend: the first dry run translated 45 of the 249 missing; 12 needed a
  capability.
- The four bending words were in the pool and `parun translate` knew none of
  them, nor Clue or Food tokens: 45 of the 204 rejections stopped at
  `_Rule:Firebending`, `_Action:Earthbend`, `_Cost:Waterbend` or an airbend
  node, and 10 at a Clue or Food token. Widening the translator for them was
  not attempted.
- Batch order: batch 3 (#7237) was blocked by batch 2 (#7236), which first
  wrote a token "with firebending N" (Cruel Administrator); batch 5 (#7239)
  by batch 1 (#7235), which first wrote "Whenever you cast a Lesson spell"
  (Aang, the Last Airbender).
- 38 batch cards carried a "verify" clause the census did not settle.

## Decks

MTGJSON lists thirteen TLA decks: ten Beginner Box decks, two six-card
box sets (Tea Time at the Jasmine Dragon, The Black Sun Invasion) and a land
pack. None is committed: decks are optional, so `data/decks/sets/tla/` holds
only the set list. It writes each of the eight transforming cards by
MTGJSON's joined name (`The Rise of Sozin // Fire Lord Sozin`).

## How it went

Finished 2026-11-13, against MTGJSON 5.3.0+20261113: the same 286 cards, every
face name in the pool.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 45 | #7245 |
| Batches | 185 | #7253 (19), #7247 (19), #7257 (16), #7250 (18), #7264 (19), #7251 (19), #7248 (18), #7258 (19), #7259 (19), #7260 (19) |
| Behind a capability | 19, plus 2 from other sets | below |

- Census trend: the first dry run translated 45 of the 249 missing; 12
  needed a capability. The batches overturned seven verdicts, each card
  leaving its batch for an issue of its own: Allies at Last, Earthbender
  Ascension, Fatal Fissure, Fire Lord Azula, Fire Navy Trebuchet,
  Planetarium of Wan Shi Tong and Watery Grasp.
- Capability issues, hardest first, then as filed: #7226 Firebender Ascension
  (#7275), #7227 Koh, the Face Stealer (#7274), #4442 Secret of Bloodbending
  (#7273), #7228 Bumi, Unleashed (#7301), #7229 The Rise of Sozin // Fire
  Lord Sozin (#7299), #7230 Destined Confrontation (#7312), #4529 Obsessive
  Pursuit and Phoenix Fleet Airship (#7314, adding Dargo, the Shipwrecker),
  #7231 Ozai, the Phoenix King (#7318, adding Horizon Stone), #7232 Diligent
  Zookeeper (#7317), #7233 The Last Agni Kai (#7319), #7234 Trusty Boomerang
  (#7330), #7246 Planetarium of Wan Shi Tong (#7311), #7249 Fire Navy
  Trebuchet (#7333), #7252 Allies at Last (#7276), #7254 Earthbender
  Ascension (#7298), #7255 Fire Lord Azula (#7290), #7256 Fatal Fissure
  (#7331), #7262 Watery Grasp (#7334).
- Filed along the way, in the set: the fix issues #7287 (a trigger Koh
  borrows can't arm its delayed trigger, #7308), #7288 (a creature's own
  "whenever you attack" uncounted, #7303), #7305 (a lent keyword's ability
  without the keyword, #7316), #7306 (a capped target total's test gap,
  #7335) and #7313 (a borrowed delayed trigger arming a second, #7324).
  Outside it: #7272 (several damage dealers with excess or redirected
  damage; open), #7278 (Master of the Wild Hunt's Wolves dealing damage one
  at a time, #7292), #7307 (a lent mana ability can't arm its card's delayed
  trigger; open, Myr Welder), #7329 (a granted ability naming its granter
  outside an effect's own object; its remainder #7341 and #7342, open).
- Audits: 12 of 18 capability PRs (#7298 inline by the orchestrator) and all
  5 fix PRs (#7335 inline); 3 found rules defects and 1 a test gap. Skipped
  the census, the ten batches (card data, renderer wording and lint specs
  only) and the capability PRs #7290, #7314, #7317, #7331, #7333 and #7334
  (card data, a lint row or one additive arm only the new cards reach), each
  recorded on #7224.
  - #7274: a triggered ability Koh borrowed that arms a delayed trigger did
    nothing (#7287, fixed by #7308).
  - #7275: a creature's own "whenever you attack" did not count for
    Firebender Ascension, which the Isshin ruling refutes (#7288, fixed by
    #7303).
  - #7308: a lent delayed trigger arming a second lost it once Koh chose
    again (#7313, fixed by #7324).
  - #7299: no rules defect; no test told the legal-target total from an
    all-target one (#7306, closed by #7335).
- Cost: 2878 turns, 413.2M cache read, 7.20M cache write, 2451k output over
  the 34 unit PRs, each of which recorded one, 710 session-minutes in all.
  The finish unit's is on its own PR. Wall clock ~6h, from the census PR's
  opening to the last capability PR's merge (#7330), then the finish.
- Owner review:
  - #7275 and #7303: decisions.md "Which abilities a creature attacking
    causes to trigger", first written on a reading the Isshin ruling refutes
    and rewritten by #7303 to the halves left open.
  - #7299 and #7335: decisions.md "A capped target set is re-totalled over
    its legal targets".
  - #7318: decisions.md "Two "that mana becomes" replacements are chosen
    between once for the pool" (Ozai with Horizon Stone).
  - #7273: two control effects on one player layer, the earlier resuming
    after the later ends (`Resolve.Common.pushControl`, from #3351), with no
    decisions.md entry; the audit judged it an arguably unforced CR 724.1a
    reading.
  - Wire-format changes: #7273 (`pendingControl` rows per duration,
    `ControlPlayerNext`), #7275 (`TriggerCondition.SagaFinalChapter` became
    `AbilityWatch`), #7299 (`TargetSlot.total`), #7311 (`SpentOn` names a
    clause), #7312 (`ChosenPermanents.total`), #7316
    (`ProjectedCharacteristics.lentKeywords`), #7324 (`DelayedTrigger`
    freezes its declarations).
  - `parun ingest` restamped two cards outside the set with MTGJSON's
    bracketed loyalty costs: Narset, Parter of Veils and Planeswalkerificate.
