# Reality Fracture census

The Reality Fracture (FRA) cards not yet in the pool, and which wait on a
capability. Derived 2026-11-06 from MTGJSON's set list against `data/cards/`.
Re-derive the missing list by writing the set list as a deck (`1 Name` per line)
and running `parun deck` on it: every unresolved name is reported.

## Expressible today

Nothing: every card `parun translate` stopped at is in the pool.































































































































































































































## Needs a capability

### #6611: no cost reduction counts something on the target of the ability being activated

- Warrior's Blades: its equip costs {1} less per +1/+1 counter on the creature
  it targets; `CostModifier.perEach` reads the source, `perTarget` only counts
  targets.

### #6607: no condition reads how many loyalty counters an activation's cost removed

- Way of the Mind Sculptor: "if you removed two or more loyalty counters to
  activate it" reads the paid loyalty cost, which nothing records.

### #6588: the next spell can't be made uncounterable without a flash grant

- Theorist's Proxy: "the next spell you cast this turn can't be countered" has
  no one-shot row for the cast to spend; only Savage Summoning's flash grant
  binds the next spell into a `CantBeCountered` row.

### #6576: no effect exempts planeswalkers from the 0-loyalty state-based action

- Sanctum Lurker: "planeswalkers you control aren't put into their owners'
  graveyards for having 0 loyalty" has no effect to express it; CR 704.5i
  buries every planeswalker at 0 loyalty.

### #6572: a delayed triggered ability can't be a mana ability

- Pyre Rhymer // Molten Tide: Molten Tide's "until end of turn, whenever you
  tap a Mountain for mana, add an additional {R}" is a delayed trigger the
  mana-ability path never scans (CR 605.1b).

### #6568: a free-cast permission can't bound mana value by a count

- Omnipresence: its free casts take spells with mana value at most the number
  of creatures you control; no filter operand reads a count of permanents.

### #6560: no effect removes up to N counters of any kind

- Mabel, Bitter Recluse: "remove up to three counters" from its target takes
  any kinds; the mixed-kind removal prompt has no cap.

### #6546: no cost exiles a card from its owner's hand

- Emrakul, the Exigent Doom: its hand ability's cost, its "until this card is
  cast from exile" and its cast permission all name the card the cost exiled.
