# Star Trek census

Star Trek (TRK) cards not yet in the pool, and which wait on a capability.
Derived 2026-11-17 from MTGJSON's set list (5.3.0+20261117) against
`data/cards/` at 0491fff2 (#8380). The set list is
`data/decks/sets/trk/all.txt`, which the suite reads, so every card of the set
stays in the pool. TRK was released 2026-11-13.

TRK has 311 card names, all single-faced (301 normal, 10 saga); 278 were
missing, 268 normal and 10 saga. mtgish had no data for 212 of them.

| Place | Cards |
| --- | --- |
| Added by the census (#8409) | 16 |
| Batches #8384 to #8395 | 245 |
| Capability issues #8396 to #8407 | 17 |

- Trend: the first dry run translated 16 of the 278 missing; 17 needed a
  capability.
- Blocked-by: shapes new to the pool were each first written by one batch
  (Officer and Borg by #8384, Romulan by #8385, "enters untapped" lands by
  #8387); the batches reusing them were linked as blocked by those. The
  census wrote the Klingon Warrior token.

## Decks

None is committed: decks are optional, so `data/decks/sets/trk/` holds only
the set list, 311 names.

## How it went

Finished 2026-11-18.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 16 | #8409 |
| Batches | 242 | #8415 (21), #8421 (21), #8416 (20), #8423 (21), #8442 (20), #8424 (20), #8429 (20), #8445 (20), #8439 (20), #8420 (20), #8444 (19), #8417 (20) |
| Behind a capability | 20, plus 1 from another set | below |

- Census trend: the first dry run translated 16 of the 278 missing; 17
  needed a capability. Batches sent three more to capabilities they filed:
  Dr. Leonard "Bones" McCoy (batch 3, #8414), Highly Illogical (batch 5,
  #8428) and U.S.S. Enterprise-D, Galaxy-Class (batch 11, #8441). Batch 6
  landed Lwaxana Troi stricter than printed and filed #8422.
- Capability issues, hardest first: #8397 The Holodeck (#8461), #8401 The
  City on the Edge of Forever (#8452), #8400 Dominion Saboteur (#8446),
  #8396 Q, Stacking the Deck (#8451), #8399 Kira Nerys, Firebrand (#8459),
  #8403 Khan, Vengeful Heart (#8476), #8398 Dr. Beverly Crusher (#8462,
  adding Kavu Predator from another set for its scenario), #8422 Lwaxana
  Troi (#8475, a false gap: re-encoded, no card added), #8402 Dr. McCoy,
  Surly Surgeon (#8465), #8428 Highly Illogical (#8466, a false gap:
  existing kicker-gated slot trimming), #8404 Dathon, His Heart Open
  (#8478), #8405 six Federation cards (#8477: Captain Jonathan Archer, Hoshi
  Sato, Exolinguist, Infinite Diversity, Saavik, Stoic Student, Sarek,
  Federation Ambassador, Starfleet Academy), #8406 H.M.S. Bounty (#8483),
  #8407 Daniels, Temporal Agent (#8485), #8414 Dr. Leonard "Bones" McCoy
  (#8491), #8441 U.S.S. Enterprise-D, Galaxy-Class (#8490).
- Fix issues: #8463 Kira Nerys's enters-attacking overrode a failed attack
  specification (#8467), #8469 The Holodeck's outside-the-game stand-ins
  counted as graveyard cards (#8470), #8486 Khan's own exile permission
  opened face-down and plotted exile casts (#8492), #8489 a plotted card
  another permission opens was offered only the free cast (#8499).
- Filed along the way, off the tracker as not TRK card work, all open:
  #8443 (a refused offered cast seems to reset the pass chain), #8449 (The
  Holodeck in a subgame), #8498 (a plotted card a CR 716.3d permission or a
  player's grant also opens).
- Audits: 12 of 20 capability or fix PRs, by subagent; 3 found rules
  defects. Reviewed inline: #8415, #8477, #8490, #8467, #8470 and #8499,
  clean. Skipped the census, the other 11 batches, #8466, #8475 and #8491
  (card data, renderer wording, lint-table entries or test-only lint
  changes), each recorded on #8379.
  - #8459: Kira's drain overrode a failed attack specification (ninjutsu,
    myriad), fix #8463.
  - #8461: stand-ins carried a graveyard zone, so Cairn Wanderer saw
    sideboard cards, fix #8469.
  - #8476: the card's own exile permission admitted a face-down exiled Khan
    and priced a same-turn plotted Khan free, fix #8486.
  - #8492: clean; filed #8489.
  - #8451, #8446, #8452, #8462, #8465, #8478, #8483, #8485: clean.
- Cost: 4496 turns, 564.5M cache read, 6.51M cache write, 2274k output over
  all 33 unit PRs, every one carrying a `Cost:` line, 655 session-minutes in
  all. The finish unit's is on its own PR. Wall clock ~7.0h, from the
  tracker's opening (2026-11-17 17:13 UTC) to the last fix PR's merge
  (#8499, 2026-11-18 00:14 UTC), then the finish.
- `data/legalities.json`: unchanged. The finish's `parun ingest` run, from
  MTGJSON of 2026-11-17, found a row for every TRK card already, written by
  the FDC finish (#8501), which merged after the last TRK card. It wrote no
  card. It stamped the missing `oracleText` of Lt. Cmdr. Geordi La Forge
  (batch 6), kept, and restamped the out-of-set Blue Mage's Cane from a
  newer wording, reverted.
- Owner review:
  - #8415: `docs/decisions.md` entry, All Good Things... chapter IV holds
    over an empty exile pile.
  - #8446: `docs/decisions.md` entry, Dominion Saboteur's copied counters
    are a CR 707.9e exception a later copy effect takes back; new
    `CopyClause.copiedCounters` field.
  - #8461: wire-format change, `LandPlayed.from` became a `CastOrigin`, and
    `CastFromZone` gained `outsideTheGame`.
  - #8465: wire-format change, the destruction rewrite became a tagged
    object (57 cards respelled).
  - #8476: wire-format change to `CastingPermission` (16 cards converted,
    the `CastFromGraveyard` tag removed).
  - #8478: wire-format change, `Effect.Proliferate` gained `Maybe
    AnotherCounter` (Powerful Broker respelled).
  - #8483: `GameState.turnsBegun` keyed by `TurnKind`; a game state saved
    before it no longer decodes.
  - #8485: wire-format change, `ExileThisFrom` became `MoveThisFrom` (50
    cards respelled).
  - Additive wire shapes, no respelling: #8452
    (`RestrictedAction.PhaseIn`), #8459 (`EntryRewrite` and
    `PendingEntryEffect` attacking), #8462 (`TriggeringMove.GainingLife`;
    an empty `entrants` is now omitted).
