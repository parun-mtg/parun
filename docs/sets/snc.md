# Streets of New Capenna census

The Streets of New Capenna (SNC) cards not yet in the pool, and which wait on
a capability. Derived 2026-11-19 from MTGJSON's set list (AllPrintings of
2026-11-18) against `data/cards/` at db7f4c4d (census #8933, tracker #8932).
The set list is `data/decks/sets/snc/all.txt`, which the suite reads, so every
card of the set stays in the pool.

SNC has 266 face names, from 469 `normal` printings; no card has a second
face, so the set list holds all 266. 226 faces were missing.

| Place | Faces |
| --- | --- |
| Translated (census PR) | 34 |
| Batches #8935 through #8943 | 185 |
| Capability issues #8944 through #8950 | 7 |

- Trend: the first dry run translated 34 of the 226 missing; 7 issues (7
  faces) needed a capability.
- No SNC card's whole text is keywords; the two planeswalkers (Elspeth
  Resplendent, Vivien on the Hunt) were hand-written (translate rejects
  `loyalty`).
- Shapes new to the pool that several batches reuse, each written first by
  one batch that the later ones were linked as blocked by: the sac-search land
  (Brokers Hideout) and the graveyard `Distinct ManaValues` count, both #8935;
  the three-colour hand-exile land grant (Glamorous Outlaw, #8938); the triome
  (Jetmir's Garden, #8939). #8945 (Evelyn) was linked as blocked by #8944
  (Lagrella), both reaching the exile-tracking filters.

## Decks

None is committed: decks are optional, so `data/decks/sets/snc/` holds only
the set list, 266 names.

## How it went

Finished 2026-11-20.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 34 | #8953 |
| Batches | 180 | #8961 (21), #8968 (20), #8959 (20), #8966 (21), #8972 (21), #8967 (20), #8979 (20), #8976 (19), #8984 (18) |
| Behind a capability | 12; 0 from other sets | below |

- Census trend: the first dry run translated 34 of the 226 missing; 7 issues
  needed a capability. Batches sent five more cards to capability issues:
  Fatal Grudge (#8958, batch 3: an edict filter's `Shares` read no slot),
  Corpse Explosion (#8965, batch 2: the slot-read lint refused a spell's
  exile-cost slot), Sizzling Soloist (#8974, batch 8: no duration over the
  controller's next combat phase), Toluz, Clever Conductor (#8980, batch 9: a
  one-or-more discard bound no cards) and Wiretapping (#8981, batch 9: a draw
  count could not be confined to a step).
- Capability issues, hardest first: #8945 Evelyn, the Covetous (#9002),
  #8944 Lagrella, the Magpie (#8989), #8974 Sizzling Soloist (#8987), #8946
  Exotic Pets (#8986), #8948 Security Rhox (#8998), #8947 Jinnie Fay,
  Jetmir's Second (#8995), #8980 Toluz, Clever Conductor (#9001), #8950 Rigo,
  Streetwise Mentor (#9003), #8981 Wiretapping (#9008), #8949 Endless Detour
  (#9004), #8958 Fatal Grudge (#9007, the engine already framed the edict:
  comments and the position lint only), #8965 Corpse Explosion (#9012, the
  lint allowance only).
- Filed along the way: #8958 by batch 3 (#8959), #8965 by batch 2 (#8968),
  #8974 by batch 8 (#8976), #8980 and #8981 by batch 9 (#8984), #8997 by the
  fix #8999, #9010 by the fix #9011, #9018 by the fix #9017. Batch 8 also
  filed #8975 (multiplayer "defending player" reads every defender), outside
  this tracker and not this set's.
- Fix and proof issues: #8992 Exotic Pets' prompt named no counter kind, and
  Security Rhox's floating mana was unproven (#9015); #8993 Sizzling Soloist's
  next-combat window and Jinnie Fay's kind across creations were unproven
  (#9000); #8996 Lagrella's return trigger missed when Lagrella was a copy as
  its ability resolved (#8999); #8997 until-return entry links lacked
  per-instance identity and kept rows no ability reads (#9005); #9006 an
  attack trigger's attacker count read power after state-based actions
  (#9013); #9009 the entry-link write gate missed a reading ability on the
  stack (#9011); #9010 the loop digest compared entry links keyed by departed
  entrants (#9017); #9014 an attack count dropped an attacker that died to a
  state-based action (#9016); #9018 the loop digest compared other id-keyed
  rows whose key had left (#9019).
- Audits: 7 of 12 capability PRs, plus the fixes #8999, #9005 and #9013, by
  subagent; 5 found rules defects, 2 of them pre-existing. Reviewed inline:
  batches #8966, #8979 and #8976, capabilities #9001, #9004, #9007, #9008 and
  #9012, and fixes #9000, #9011, #9015, #9016, #9017 and #9019, clean.
  Skipped the census and the other six batches (card data, scenarios and
  renderer wording). Each is recorded on #8932.
  - #8986: no defect; the per-kind prompt named no counter kind, filed #8992.
  - #8987: no defect; two proof gaps and a misplaced decisions.md entry,
    filed #8993.
  - #8989: a Lagrella copying something as its ability resolved never
    triggered, the link and the delayed trigger recording different
    acquisitions; filed #8996.
  - #8995: no defect; a proof gap, folded into #8993.
  - #8998: no defect; a proof gap, folded into #8992.
  - #8999: no defect in the change; a pre-existing one from #8989 (two
    Lagrellas each counted the other's returns), folded into #8997.
  - #9002: clean.
  - #9003: no defect in the change; a pre-existing one Rigo reaches (an
    attack count's filter read power after state-based actions), filed #9006.
  - #9005: the new write gate was blind to a reading ability on the stack (a
    Clone of Tetravus turned into Bears mid-stack lost its link), filed #9009.
  - #9013: membership was still read after state-based actions (a 0/0
    Hangarback Walker attacker left Rigo untriggered), filed #9014.
- Cost: 460.5M cache read, 5.57M cache write, 1899k output over all 31 unit
  PRs, and 3592 turns over the 30 that record turns (#9012 does not), 750
  session-minutes in all. The finish unit's is on its own PR. Wall clock
  ~10.9h, from the tracker's opening (2026-11-19 20:45 UTC) to the last unit's
  merge (#9019, 2026-11-20 07:37 UTC), then the finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-17. It gained 233 rows and lost none: the 226 SNC names
  the run added, and seven cards #8931 added (Brightclimb Pathway, Delver of
  Secrets, Isochron Scepter, Knowledge Pool, Teferi's Protection, The One
  Ring, Worldgorger Dragon). The other 40 SNC names, reprints, already had
  rows. Every SNC name now has a row. It wrote no card.
- Owner review:
  - #8987: a `docs/decisions.md` entry, "Whose combat phase its controller's
    next combat phase is after a control change".
  - #9000: a `docs/decisions.md` entry, "Whether a combat phase under way is
    its controller's next" (it is not).
  - #9003: a `docs/decisions.md` entry, "How often Rigo draws for a player and
    their planeswalker" (once per player or planeswalker attacked).
  - Wire-format changes respelling cards: #8987 (the next-combat window
    became a parameter on the existing `Duration` and `Expiry` windows; 14
    cards respelled), #8995 (`TokenPlus.Instead` carries a list of cards;
    Draconic Visitor and Holo-Engineer respelled), #9003
    (`AttackedSubject.Related` takes its attack-target kinds, defaulting to
    the player alone; 13 cards respelled).
  - Codec changes respelling no card: #8986 (`PutCounterOfEachKind`),
    #8989 (`ReturnWatch`), #8998 (`ManaSpecification.ProducedBy`,
    `AlternativeCost.spendOnly`), #9002 (`ExileLink`'s exiling controller),
    #9005 (`SourceChoices` and `EntryLink` keys), #9008
    (`PlayerDrawsNthCard.within`), #9004
    (`GraveyardPartner.SpellsAndPermanents`).
