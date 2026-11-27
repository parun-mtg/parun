# Archenemy schemes census

The Archenemy schemes not yet in the pool, and which wait on a capability.
Derived 2026-11-26 from MTGJSON's scheme names (AllPrintings of 2026-11-25)
against `data/cards/` at 0f4ec393 (census #9928, tracker #9927).

The tracker is scoped by card type, not set code (`docs/agents/orchestrating.md`,
"product line"): it covered every scheme (CR 314) across Archenemy Schemes
(OARC, 45 names), Archenemy: Nicol Bolas Schemes (OE01, 20), Duskmourn: House
of Horror Commander (DSC, 40) and DCI Promos (DCI, 5), with overlaps. It
commits the set lists of OARC and OE01 only, which are schemes only:
`data/decks/sets/oarc/all.txt` and `data/decks/sets/oe01/all.txt`, which the
suite reads, so every card of both sets stays in the pool. DSC and DCI keep
their own runs, whose censuses find the schemes already present.

There are 102 scheme names; 7 were in the pool, 95 missing. Missing by first
printing: OARC 40, OE01 19, DSC only 32, DCI only 4.

| Place | Cards |
| --- | --- |
| Translated (census PR #9933) | 4 |
| Batches #9934, #9935, #9936, #9937, #9938 | 81 |
| Capability issues | 10 |

- Trend: the first dry run translated 0 of the 95 missing (translate read no
  scheme card); with #9933's widening, 4. 10 needed a capability.
- Capability issues, hardest first: #9939 (Bow to My Command, Imprison This
  Insolent Wretch), #9940 (Choose Your Champion), #9941 (I Am Never Alone),
  #9942 (Only Blood Ends Your Nightmares), #9943 (Only I Know What Awaits),
  #9944 (The Mighty Will Fall, Your Inescapable Doom), #9945 (Mine Is the
  Only Truth), #9946 (Running Is Useless).

## Decks

None is committed: decks are optional, so `data/decks/sets/oarc/` and
`data/decks/sets/oe01/` hold only the set lists, 45 and 20 names.

## How it went

Finished 2026-11-27.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 4 | #9933 |
| Batches | 78 | #9947, #9951, #9955, #9952, #9954 |
| Behind a capability | 13, plus 1 from other sets | below |

- Census trend: the first dry run translated 0 of the 95 missing (4 once
  #9933 widened `candidates.jq` and `parun translate` to read scheme cards);
  10 needed a capability. The batches moved 3 cards to capabilities: Nature
  Demands an Offering (#9951, to #9950), My Champion Stands Supreme (#9952,
  to #9949) and You Will Know True Suffering (#9954, to #9948). Three
  capabilities were card data only, the verdict overturned: #9942, #9948
  and #9950.
- Capability issues, in landing order: #9942 Only Blood Ends Your
  Nightmares (#9956), #9941 I Am Never Alone (#9957), #9944 The Mighty Will
  Fall and Your Inescapable Doom (#9958), #9949 My Champion Stands Supreme
  (#9959), #9946 Running Is Useless (#9961), #9943 Only I Know What Awaits
  (#9960), #9945 Mine Is the Only Truth (#9964), #9948 You Will Know True
  Suffering (#9967), #9950 Nature Demands an Offering (#9966), #9940 Choose
  Your Champion (#9976), #9962 My Forces Are Innumerable's joint cost, a
  `rules-correctness` fix to a batch card (#9977), #9939 Imprison This
  Insolent Wretch (#9965) and Bow to My Command (#9980). The card from
  another set is Sawhorn Nemesis (M3C), #9978's printed writer of a chosen
  player as damage recipient.
- Filed along the way: #9949, #9948 and #9950 by batches (above); #9962 by
  #9965 (an opposing team's jointly paid end-step cost); #9963 by #9964
  (translate and render "abandon this scheme", #9972). #9971 ("you may
  abandon this scheme. If you do", filed by #9972) was moved out from under
  the tracker, since both its cards are in the pool. #9979 (the enchanted
  player as damage recipient, Curse of Bloodletting) was filed outside it by
  #9978's audit.
- Fix issues, from audits: #9968 (#9969), #9970 (#9974), #9973 (#9978),
  #9981 (#9982).
- Audits: 7 of 13 capability PRs (#9959, #9964, #9965, #9966, #9976, #9977,
  #9980), and fix #9978; none found a rules defect. Each audit, inline
  review and skip is recorded on #9927.
  - #9959: no scenario separated the commander record from a copy, nor
    owner from controller; #9968.
  - #9964: the opening-hand exclusion had no proving scenario, and the
    unprinted since-your-last-turn tallies no lint; #9970.
  - #9965: chosen-player constructors were siblings of the enchanted-player
    ones, with missing trigger pins and an unfenced relation; #9973.
  - #9977: a mutation reddened the wrong check, "triggers once" was
    unproven, and no lint kept a joint payer to one team; folded into #9939.
  - #9980: the total-power threshold of 8 was unpinned; #9981.
- Cost: 335.3M cache read, 4.08M cache write, 1.31M output and 2625 turns
  (one PR, #9982, recorded no turns) over the 24 unit PRs, about 524
  session-minutes in all; the duplicate session the label routine started
  on #9968 is not recorded. The finish unit's is on its own PR. Wall clock
  ~12.4h, from the tracker's opening (2026-11-26 19:42 UTC) to the last
  unit's merge (#9982, 2026-11-27 08:03 UTC), then the finish.
- `data/legalities.json`: regenerated by the finish's `parun ingest`, from
  MTGJSON of 2026-11-24. It gained 96 rows and lost none: the 95 missing
  scheme names (each empty, as MTGJSON lists no format for a scheme; the 7
  already in the pool had empty rows) and Sawhorn Nemesis. No existing row
  changed. It wrote no card. It restamped `oracleText` on two out-of-set
  cards, Hit It! and B'Elanna Torres, Scrapper, as earlier finishes saw;
  both reverted, since their abilities still say the old wording.
- Owner review:
  - `docs/decisions.md`: no scheme PR added or changed an entry.
  - #9977: CR 805.2 gives a team's disputed choice to its primary player,
    who picks the team's joint payment (My Forces Are Innumerable, Bow to My
    Command).
  - Wire-format changes: `Superlative.among` (#9958); a commander record in
    `LastKnown` (#9959); `ChosenPermanents` takes `DifferentIn` (#9961);
    `ChosenCardInHand` takes `onePer` (#9960); `CardsDrawn` names its turn,
    18 cards rewritten (#9964); `ChoosePlayer.linked` and the chosen-player
    filters, five cards renamed (#9965); `PlayerRef.HoldersOfBound` takes a
    list of slots (#9966); `ChoosePlayer.chooser` and `AffectedPlayers.AllBut`
    (#9976); `PayGate.grouping` (#9977); one `NamedBySource SourcePlayer`
    across four types, 17 cards respelled (#9978); `SetInMotionR` and a
    joint `TapForTotalPower` (#9980).
