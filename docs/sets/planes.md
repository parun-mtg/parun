# Planechase planes census

The Planechase planes and phenomena not yet in the pool, and which wait on a
capability. Derived 2026-11-27 from MTGJSON's plane and phenomenon names
(AllPrintings of 2026-11-25, re-fetched 2026-11-27, unchanged for planes)
against `data/cards/` at a7f064d4 (census #9990, PR #9991, tracker #9989).

The tracker is scoped by card type, not set code (`docs/agents/orchestrating.md`,
"product line"): it covered every plane (CR 311) and phenomenon (CR 312)
across Planechase Planes (OHOP, 40 names), Planechase 2012 Planes (OPC2, 40),
Planechase Anthology Planes (OPCA, 86), March of the Machine Commander (MOC)
and Doctor Who (WHO), and their promo reprints (DCI, PHOP, PRM), with
overlaps. It commits the set lists of OHOP, OPC2 and OPCA only, which are
planes and phenomena only: `data/decks/sets/ohop/all.txt`,
`data/decks/sets/opc2/all.txt` and `data/decks/sets/opca/all.txt`, which the
suite reads, so every card of the three sets stays in the pool. MOC, WHO and
the deck sets (HOP, PC2, PCA) keep their own runs, whose censuses find the
planes already present. Out of scope, the owner's call: Black Lotus Unknown
Planechase (OUNK) and Secret Lair Showcase Planes (PSSC), and every name
printed only there.

There are 151 names; 33 were in the pool, 118 missing. Missing by first
printing: OHOP 31, OPC2 30, OPCA 4, MOC 20, WHO 33. Seven of the missing are
phenomena.

| Place | Cards |
| --- | --- |
| Translated (census PR #9991) | 5 |
| Batches #9992, #9993, #9994, #9995, #9996 | 88 |
| Capability issues | 25 |

- Trend: the first dry run translated 0 of the 118 missing (mtgish's `Planar`
  kind was unread); with #9991's widening, 5. 25 needed a capability.
- Capability issues, hardest first: #9997 (Two Streams Facility), #9998
  (Glimmervoid Basin), #9999 (Human—Time Lord Meta-Crisis), #10000 (Talon
  Gates), #10001 (The Matrix of Time), #10002 (TARDIS Bay), #10003 (Skybreen),
  #10004 (Glen Elendra), #10005 (Chaotic Aether), #10006 (Ood Sphere), #10007
  (Bad Wolf Bay), #10008 (Norn's Seedcore), #10009 (Singing Towers of
  Darillium), #10010 (Edge of Malacol), #10011 (Sea of Sand), #10012
  (Stensia), #10013 (Furnace Layer), #10014 (Bowie Base One, Caught in a
  Parallel Universe), #10015 (Nyx, Hotel of Fears), #10016 (Stairs to
  Infinity, Ten Wizards Mountain, The Drum, Mining Facility, Pompeii).

## Decks

None is committed: decks are optional, so `data/decks/sets/ohop/`,
`data/decks/sets/opc2/` and `data/decks/sets/opca/` hold only the set lists,
40, 40 and 86 names.

## How it went

Finished 2026-11-28.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 5 | #9991 |
| Batches | 84 | #10024, #10020, #10023, #10026, #10025 |
| Behind a capability | 29 | below |

- Census trend: the first dry run translated 0 of the 118 missing (5 once
  #9991 taught `parun translate` to read plane and phenomenon cards); 25
  needed a capability. The batches moved 4 cards to capabilities: The Dark
  Barony (#10024, to #10018), Horizon Boughs (#10023, to #10022), The Golden
  City of Orazca and The Pit (#10026, to #10017 and #10019). Two capabilities
  were card data only, the verdict overturned: #9999 and #10001.
- Capability issues, in landing order: #9999 Human—Time Lord Meta-Crisis
  (#10027), #10001 The Matrix of Time (#10028), #10003 Skybreen (#10029),
  #9998 Glimmervoid Basin (#10031), #10002 TARDIS Bay (#10030), #9997 Two
  Streams Facility (#10033), #10007 Bad Wolf Bay (#10035), #10004 Glen
  Elendra (#10034), #10000 Talon Gates (#10032), #10018 The Dark Barony
  (#10039), #10010 Edge of Malacol (#10046), #10019 The Pit (#10047), #10006
  Ood Sphere (#10038), #10008 Norn's Seedcore (#10036), #10009 Singing Towers
  of Darillium (#10044), #10022 Horizon Boughs (#10058), #10011 Sea of Sand
  (#10071), #10012 Stensia (#10074), #10013 Furnace Layer (#10078), #10017
  The Golden City of Orazca (#10079), #10015 Nyx and Hotel of Fears (#10080),
  #10016 Stairs to Infinity, Ten Wizards Mountain, The Drum, Mining Facility
  and Pompeii (#10081), #10014 Bowie Base One and Caught in a Parallel
  Universe (#10083), #10005 Chaotic Aether (#10089, merged without a
  self-review or a closing keyword; #10005 closed by hand).
- Filed along the way: #10018, #10022, #10017 and #10019 by batches (above);
  #10045 by #10047 (either-or branches announced by different seats happen
  as one event) and #10052 from #10032's audit (a `ForEach` order nothing
  observes), both moved off the tracker as related; #10070 by #10071 (`expires:synthetic`,
  a face-down draw paid away before the cast), moved off and open; #10076 by
  #10078 (random object targets), moved off, its cards landing in #10106;
  #10087 by #10081's audit (an effect that rolls the planar die, Fractured
  Powerstone), filed outside, landing in #10105; #10090 by #10091 (a
  teammate's planeswalk-to instance); #10093 by #10091's audit
  (`needs-planning`, the Two-Headed Giant planar model); #10109 by #10103's
  audit (whose hand Pools of Becoming reads).
- Fix issues, from audits: #10037 (#10041), #10040 (#10049), #10042
  (#10050), #10043 (#10051), #10048 (#10056), #10053 (#10082), #10054
  (#10059), #10055 (#10072), #10057 (#10075), #10073 (#10077), #10084
  (#10088), #10085 (#10091), #10086 (#10102), #10090 (#10092), #10093
  (#10103), #10108 (#10111), #10109 (#10114).
- Audits: 17 of 24 capability PRs (#10030, #10031, #10032, #10033, #10034,
  #10035, #10036, #10038, #10044, #10046, #10047, #10074, #10078, #10080,
  #10081, #10083, #10089), and fixes #10059, #10091, #10102 and #10103; 8
  capability PRs had rules defects. #10111 (the last Two-Headed Giant fix)
  was not audited: the owner closed the run first. Each audit, inline review
  and skip is recorded on #9989.
  - #10030: no rules defect, but the turn-scope conjunct under CR 901.6's
    heir was unproven, and its decisions entry was forced by a ruling;
    #10037.
  - #10033: departed seats chose a word, and Grand Melee's later starting
    planes missed the first upkeep; #10040.
  - #10034: the this-combat window read another Grand Melee turn's combat;
    #10043.
  - #10035: melding from exile skipped the prohibition; #10042.
  - #10038: an impossible tap was still offered; #10057.
  - #10044: a foretold card could be cast for other costs under the free
    grant; #10073.
  - #10046: untap-replacement counters landed after the upkeep's first
    state-based actions, #10053; a plane's replacement "you" read the planar
    controller alone, #10054.
  - #10074: earlier noncreature damage spent Stensia's first time, and its
    actor was not confined to the battlefield; #10084.
  - #10081: a you-less planar trigger fired per Two-Headed Giant member,
    #10085; a reroll was not a roll of the planar die, #10086.
  - #10032 and #10036: test gaps only; #10048 and #10055.
  - #10091: its "refers to you" classification followed data defaults, not
    the text; #10093. #10103: four planes contradicted the decision; #10108.
- Cost: 743.8M cache read, 9.03M cache write, 3.02M output and 5914 turns
  (one PR, #10031, recorded no turns) over the 47 unit PRs, about 1049
  session-minutes in all. #10105 and #10106, landing issues moved off the
  tracker, are not counted. The finish unit's is on its own PR. Wall clock
  ~13.9h, from the tracker's opening (2026-11-27 10:58 UTC) to the last
  unit's merge (#10114, 2026-11-28 00:54 UTC), then the finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-24. It gained 123 rows and lost none: the 118 missing
  plane and phenomenon names (each empty, as MTGJSON lists no format for a
  plane), and five cards from other sets added since the schemes finish:
  Curse of Bloodletting (#9987), Fractured Powerstone (#10105), Goblin Test
  Pilot, Scab-Clan Giant and Power Pack (#10106). No existing row changed.
  It wrote no card. It restamped `oracleText` on two out-of-set cards, Hit
  It! and B'Elanna Torres, Scrapper, as earlier finishes saw; both reverted,
  since their abilities still say the old wording.
- Owner review:
  - `docs/decisions.md`: entries added by #10030 (replaced by a ruling in
    #10041), #10049 ("Which upkeep is the first of a Grand Melee game"),
    #10074 (deleted in #10088), #10075 ("Whether a permanent that can't
    become tapped phases in tapped from Oubliette"), #10080 ("What Hotel of
    Fears' another creature excludes"), #10083 ("Whether the player to your
    left closes up over a seat that emptied"), #10089 ("Whether Chaotic
    Aether's chaos roll is still a blank roll") and #10102 (retitled by the
    finish "Which face of a rerolled planar die a trigger reads"); #10029
    extended the held-back top card entry; the planar-team entry was
    narrowed by #10091 and rewritten by #10103.
  - Owner decisions: #10093, a planar ability exists once in Two-Headed
    Giant, its "you" the planar team (#10103); #10109, Pools of Becoming
    reads the planar controller's own hand (#10114).
  - #10059: as teammate T, Edge of Malacol's counters are put by T, under
    the planar-team entry, which names choices, not the putter.
  - Wire-format changes: `AnchorPool.LibraryTops` (#10029); `ObjectRef`'s
    owned-card refs as `EachCard`, 9 cards rewritten (#10031);
    `Affected.MatchingOffBattlefield` takes `onlyNth`, 71 cards rewritten
    (#10030); `SourcePlayer` enum to tagged, 19 cards migrated, and
    `TurnScope.FirstTurnOfGame` (#10033); a this-combat `Lookback` and an
    `Actor` slot dealer (#10034); `Prohibition.EnterFrom` (#10035);
    `SpecialAction` with `PerformAsASorcery` (#10032); `UntapRewrite`,
    `UntapR` and a `ReplacementEffect` arm (#10046); `Prohibition.TapExceptAttacking`
    (#10038); `Effect.Planeswalk`'s payload (#10036); `Filter.HasExileMark`
    replacing `IsWarped` (#10044); `UntapHold.DuringOthersSteps` takes
    `AnyPlayer` (#10058); a `DamageTrigger` frequency (#10074); a random
    `TargetSlot` selection (#10078); `DevotionColors`, 7 devotion cards
    migrated (#10080); `PlanarDieTrigger` (#10081); `ControllerRelation`'s
    player to the left (#10083); `DurationEvent.Planeswalked` takes
    `PlaneswalkedFrom`, and `BlankPlanarDieRollsAreChaos` (#10089); a
    faceless `PlanarDieRolled` for a reroll (#10102).
