# March of the Machine: The Aftermath census

March of the Machine: The Aftermath (MAT) cards not yet in the pool, and which
wait on a capability. Derived 2026-11-18 from MTGJSON's set list
(5.3.0+20261117) against `data/cards/` at 2e59028b. The set list is
`data/decks/sets/mat/all.txt`, which the suite reads, so every card of the set
stays in the pool. MAT was released 2023-05-12.

MAT has 50 card names, all normal layout; 42 were missing.

| Place | Cards |
| --- | --- |
| Added by the census (#8518) | 1 |
| Batches #8514, #8515 | 39 |
| Capability issues #8516, #8517 | 2 |

- Trend: the first dry run translated 1 of the 42 missing; 2 needed a
  capability.
- Blocked-by: no shape new to the pool was shared across the batches, so
  neither blocked the other; the two capabilities touched different edit
  sites (trigger binding; activation restrictions).

## Decks

None is committed: decks are optional, so `data/decks/sets/mat/` holds only
the set list, 50 names.

## How it went

Finished 2026-11-18.

| Route | Cards | PRs |
| --- | --- | --- |
| Translated (census) | 1 | #8518 |
| Batches | 39 | #8553 (20), #8558 (19) |
| Behind a capability | 2 | below |

- Census trend: the first dry run translated 1 of the 42 missing; 2 needed
  a capability. Every census verdict held: neither batch filed a capability.
- Capability issues, hardest first: #8516 Arni Metalbrow (#8567), #8517
  Tazri, Stalwart Survivor (#8569).
- Fix issues: #8578 Tazri's restricted mana paid for abilities of creature
  cards off the battlefield (#8586, which also confined Renowned Weaponsmith,
  Hargilde, Kindly Runechanter, Slobad, Iron Goblin and Orb of Dragonkind).
- Filed along the way: none.
- Audits: 1 of 3 capability or fix PRs, by subagent; 1 found a rules
  defect. Reviewed inline: #8567, clean. Skipped the census, both batches
  and #8586 (card data, renderer wording), each recorded on #8509.
  - #8569: Tazri's restricted mana paid for abilities of creature cards in
    hand or graveyard, fix #8578.
- Cost: 1072 turns, 102.1M cache read, 1.64M cache write, 518k output over
  all 6 unit PRs, every one carrying a `Cost:` line, 128 session-minutes in
  all. The finish unit's is on its own PR. Wall clock ~3.3h, from the
  tracker's opening (2026-11-18 01:54 UTC) to the fix PR's merge (#8586,
  2026-11-18 05:12 UTC), then the finish.
- `data/legalities.json`: the finish's `parun ingest` run, from MTGJSON of
  2026-11-17, added 133 rows: all 42 MAT cards and 91 cards other units
  added since the TRK finish (#8503). It wrote no card. It restamped the
  out-of-set Blue Mage's Cane from a newer wording, reverted.
- Owner review: no `docs/decisions.md` entry and no respelling wire
  change. Additive wire shapes: #8569 (`Measure.ActivatedAbilityCount`).
