# Lorwyn Eclipsed census

The Lorwyn Eclipsed (ECL) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-12 from MTGJSON's set list against `data/cards/`
(#7102). The set list is `data/decks/sets/ecl/all.txt`, which the suite reads,
so every card of the set stays in the pool.

ECL has 273 cards (MTGJSON 5.3.0+20261112; each of five shock lands is listed
twice, once as a reversible card, and counted once); 35 were already in the
pool, 238 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#7128) | 37 |
| Batches #7119 to #7127 | 183 |
| Capability issues #7109 to #7118 | 18 |

- Trend: the first dry run translated 37 of the 238 missing; 18 needed a
  capability.
- Vivid was in the pool already (Volatile Rift); Bloom Tender (#7116) and
  Sanar (#7110) were the vivid cards needing more.
- 48 batch cards carried a "verify" clause the census did not settle.

## Decks

MTGJSON lists two theme decks (Angels, Pirates), two land packs and the MTGO
redemption list. None is committed: decks are optional, so
`data/decks/sets/ecl/` holds only the set list. It writes each of the seven
transforming cards by MTGJSON's joined name (`Oko, Lorwyn Liege // Oko,
Shadowmoor Scion`), and each shock land once.

## How it went

Finished 2026-11-13.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 37 | #7128 |
| Batches | 171 | #7130 (21), #7135 (21), #7146 (19), #7137 (18), #7147 (18), #7134 (19), #7154 (20), #7141 (17), #7153 (18) |
| Behind a capability | 30, plus 4 from other sets | below |

- Census trend: the first dry run translated 37 of the 238 missing; 18
  needed a capability. The batches overturned twelve verdicts, each filed as
  a capability issue below.
- Capability issues, hardest first, then as filed: #7109 Mirrormind Crown
  (#7160, adding Esix, Fractal Bloom), #7110 Sanar, Innovative First-Year
  (#7155), #7111 Celestial Reunion (#7165), #7112 Oko, Lorwyn Liege (#7175),
  #7113 Burning Curiosity, Cinder Strike, Pyrrhic Strike and Requiting Hex
  (#7159), #7114 Catharsis, Deceit, Emptiness, Vibrance and Wistfulness
  (#7156), #7115 Dawn-Blessed Pennant and Eclipsed Realms (#7158), #7116
  Bloom Tender (#7176), #7117 Mornsong Aria (#7190, adding Maralen of the
  Mornsong), #7118 Meek Attack (#7188); from the batches, #7131 Goliath
  Daydreamer (#7163), #7139 Spinerock Tyrant (#7168), #7132 Harmonized
  Crescendo, #7133 Mutable Explorer and #7140 Spiral into Solitude and Sygg,
  Wanderwine Wisdom (#7191, one PR for the three), #7142 Figure of Fable
  (#7195, adding Cliffside Rescuer), #7143 Foraging Wickermaw (#7182), #7144
  Lasting Tarfire (#7180), #7145 Maralen, Fae Ascendant (#7170), #7151
  Twilight Diviner (#7174), #7152 Winnowing (#7164).
- Fix issues, from audits: #7161 (a text change did not reach a subtype
  choice's list, #7169), #7162 (Mirrormind Crown's first creation misread on
  three boards, #7166), #7167 (Goliath Daydreamer's counter scaled, and "from
  your hand" read as any hand, #7172), #7171 (the Crown offered again after
  a creation that created nothing, #7179), #7177 (Twilight Diviner's mixed
  batch and its caster, #7181, adding Ecological Appreciation), #7178 (milled
  cards exiled instead were not found, #7183), #7184 (Lasting Tarfire counted
  a counter put off the battlefield, #7185), #7186 (Foraging Wickermaw's
  colour outlived its ability, #7189), #7193 (an optional draw offered to a
  player who can't draw, #7194).
- Filed along the way and moved off the tracker, each open: #7157 (an
  activated ability's mode count reading its own cost), #7173 (a prompt
  naming a departed object), #7187 (a colour chosen on resolution read as
  another ability's linked choice, Myr Welder only).
- Process changes during the set: #7158 (`docs/agents/set-by-set.md`, run
  `-p Parun.Ingest` for a hand-written card), #7185 (`CLAUDE.md`, no
  `IsInZone Battlefield` on a counter-put holder or look-back), #7192
  (`script/wait-pr.sh` reads over REST where GraphQL is refused).
- Audits: 16 of 19 capability PRs and 5 of 9 fix PRs; 9 found rules
  defects. Skipped the census and all nine batches (card data, renderer and
  lint specs), the capability PRs #7176, #7188 and #7191, and the fix PRs
  #7169, #7183, #7185 and #7189, each recorded on #7101.
  - #7158: a text change did not reach the printed list of types (#7161).
  - #7160: a token mutated over, a diverted creation and two copying rows in
    one batch (#7162).
  - #7163: Vorinclex scaled the dream counter, and the trigger read any hand
    (#7167).
  - #7166: a creation whose tokens were not created left the Crown unspent
    (#7171).
  - #7174: the synthetic-only reading was observable on a printed card, and
    "cast from a graveyard" was read as cast by you (#7177).
  - #7175: "from among them" read only graveyards (#7178).
  - #7180: the look-back had no battlefield test (#7184).
  - #7182: the chosen colour was never cleared (#7186).
  - #7190: an optional clause bundling a draw was still offered (#7193).
- Leads audits raised that were not filed, each unverified: an optional cost
  is offered only if payable at announcement, where a mana ability drawing a
  card during payment could make it payable (#7165); inside a batch the
  would-create note sits in scratch that the "when you do" comparison still
  reads (#7179; no pool card has the shape); an optional connive is still
  offered to a player who can't draw (#7194; no ruling); Winnowing's
  scenarios cannot tell a controller-keyed read from an owner-keyed one
  (#7164); a resolution-granted cast permission with a count would read it
  live (#7170; no card writes one).
- Out-of-set cards sharing a new wording, not added: Lord Jyscal Guado and
  Sigardian Paladin (#7180), Courageous Resolve (#7195), Dauthi Voidwalker
  (#7163), The Five Doctors (#7181).
- Oracle text: several units stamped `oracleText` by hand from Scryfall; the
  finish's `parun ingest` run named no pool card as disagreeing with MTGJSON.
- Cost: 3001 turns, 430.1M cache read, 7.9M cache write, 2447k output over
  all 38 unit PRs, 1499 session-minutes in all; #7191's transcript may be
  partial after a worker restart. The finish unit's is on its own PR. Four
  late units (#7188 to #7191) report over three hours each, a cold build in
  one and otherwise `script/wait-pr.sh` waiting out a refused GraphQL call
  (#7192), not work. Wall clock ~13h, from the tracker's opening to the last
  unit's merge (#7195), then the finish.
- Owner review:
  - #7160: decisions.md "Whether a replaced copy effect's exceptions reach
    Mirrormind Crown's copies".
  - #7179: decisions.md "Whether a token creation that creates nothing
    spends Mirrormind Crown's first time"; it also replaced the `Moved`
    field #7166 had added with a `TurnRegisters` field.
  - #7181: decisions.md, Twilight Diviner's "they" in a batch only partly
    from a graveyard, moved there from `docs/synthetic-readings.md` once
    Ecological Appreciation was found to build the batch; `WasCastFrom` now
    names no caster unless the card adds one.
  - Wire-format changes: #7158 (`ChosenQuality.Subtype` is an object with an
    optional list; 28 cards rewritten), #7164 (`Anchor.Bound` is an object;
    29 anchors rewritten), #7156 (`ManaSpent` takes a cost portion, replacing
    `OfTypeOnX`), #7190 (`PlayerEffect`'s can't family is one constructor
    over `ForbiddenAct`; five cards rewritten), #7183 (six mill-then-take
    cards read the mill's slot).
