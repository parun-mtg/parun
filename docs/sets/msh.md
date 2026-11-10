# Marvel Super Heroes census

The Marvel Super Heroes (MSH) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-09 from MTGJSON's set list against `data/cards/`
(#6777). The set list is `data/decks/sets/msh/all.txt`, which the suite reads,
so every card of the set stays in the pool.

MSH has 281 cards (MTGJSON); 23 were already in the pool, 258 missing.

| Place | Cards |
| --- | --- |
| Added by the census (#6807) | 26 |
| Batches #6797 to #6806 | 198 |
| Capability issues #6778 to #6796 | 24 |
| Behind open issues elsewhere (#6195, #6103, #6187, #6080, #5783, #3906) | 10 |

- Trend: the first dry run translated 26 of the 258 missing; 34 needed a
  capability.

## Decks

None is committed: decks are optional, so `data/decks/sets/msh/` holds only
the set list. It writes each double-faced card by MTGJSON's joined name, which
resolves.

## How it went

Finished 2026-11-10.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 26 | #6807 |
| Batches | 188 | #6826 (19), #6824 (19), #6828 (19), #6822 (20), #6829 (19), #6841 (19), #6833 (18), #6838 (20), #6840 (19), #6837 (16) |
| Behind a capability | 44, plus 10 from other sets | below |

- Census trend: the first dry run translated 26 of the 258 missing; 34
  needed a capability.
- Capability issues, census order (hardest first), then as filed: #6778
  Baron Helmut Zemo (#6847), #6779 Storm, Windrider (#6843), #6780 The
  Kingpin of Crime (#6844), #6781 Leader, Super-Genius (#6853), #6782 Baron
  Strucker, HYDRA Overlord and Jennifer Walters (#6856), #6783 Agent Maria
  Hill (#6852), #6784 Wonder Man, Hollywood Hero (#6861), #6785 Beast,
  Erudite Aerialist and Kid Loki (#6860), #6786 Avengers Assemble! and Iron
  Man, Master of Machines (#6863), #6787 The Ruinous Wrecking Crew (#6865),
  #6788 Bullseye, Death Dealer and Titania, Rugged Rumbler (#6866), #6789 The
  Serpent Society (#6862), #6790 The Thing, Ben Grimm (#6868), #6791 Loki,
  God of Mischief (#6869), #6792 Powerful Broker (#6871), #6793 Spider-Woman,
  Secret Agent (#6872), #6794 Captain America, Living Legend (#6870), #6795
  Crowd of True Believers (#6874), #6796 Hawkeye, Young Avenger (#6877);
  #6820 Attuma, Atlantean Warlord (#6876), #6823 Call Damage Control (#6878),
  #6825 Luke Cage, Power Man and The Unbeatable Squirrel Girl (#6875), #6827
  Ghost, Spectral Saboteur and Quake, Agent of S.H.I.E.L.D. (#6879), #6832
  Pym Particles (#6880), #6834 Nick Fury, Agent of S.H.I.E.L.D. (#6882),
  #6835 Vision Quest (#6881), #6836 World War Hulk (#6883), #6864 Hawkeye,
  Master Marksman (#6889).
- From elsewhere: #6195 Widow's Bite (#6855), #6103 (#6849, adding Spinal
  Parasite, Thrull Parasite and Ferropede), #6187 and #6850 Namor the
  Sub-Mariner (#6854 adding Zaxara, the Exemplary, Rage Extractor and Gaddock
  Teeg; #6892), #6080 Shang-Chi, Master of Kung Fu (#6845, adding
  Thousand-Year Elixir), #5783 (#6846, adding Monument to Endurance), #3906
  Wolverine, Fierce Fighter (#6842). Each of #6855, #6849 and #6846 added only
  its proving card, leaving Atlantis Attacks, Go Nuts!, HULK SMASH!, Murdock's
  Crusade, Mister Hyde, Monster Within and The Vision to the finish (#6899).
- Filed along the way: #6850 (Namor's blue mana symbols), #6858 (an activated
  ability's computed mode count, fixed by #6890 with a synthetic card), #6859
  (a look-back count reading current characteristics, fixed by #6893), #6873
  (attacking a player alone, fixed by #6891 with Jin Sakai, Ghost of
  Tsushima), #6899 (the six cards the finish found).
- Audits: not recorded.
- Cost: not recorded. Wall clock ~21h, from the census PR's opening to the
  last capability PR's merge (#6892), then the finish.
- Owner review:
  - #6843: decisions.md "Which objects a cast trigger's "those creatures"
    names".
  - #6861: decisions.md "Whether two "an additional time" grants stack".
  - #6876: decisions.md "Whom Attuma's Merfolk attack".
  - #6891: decisions.md "What Jin Sakai's "no other creatures are attacking
    that player" asks".
