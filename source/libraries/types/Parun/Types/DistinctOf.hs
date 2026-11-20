module Parun.Types.DistinctOf where

-- | Which characteristic Parun.Types.Aggregation's Distinct counts the distinct
-- values of among the members: the size of the union of each member's set.
data DistinctOf
  = -- | CR 205.2a: card types (CR 208.2a's Tarmogoyf).
    CardTypes
  | -- | CR 205.2a: the card types the object the count is evaluated against
    -- has too (Cemetery Prowler).
    CardTypesShared
  | -- | CR 110.4: permanent types, a member's other card types adding none
    -- (Matzalantli, the Great Door).
    PermanentTypes
  | -- | CR 105.1: colors, a colorless member adding nothing (CR 105.2c).
    Colors
  | -- | CR 105.2 / 105.5: color combinations, each member's whole set of
    -- colors one value (Niv-Mizzet, Guildpact's color pairs).
    ColorSets
  | -- | CR 205.3i / 305.6: Forest, Island, Mountain, Plains and Swamp, the
    -- domain count ("the number of basic land types among lands you control").
    BasicLandTypes
  | -- | CR 205.3i: the land types other than the five basic ones, Mojave
    -- Desert's "the number of nonbasic land types among lands that player controls".
    NonbasicLandTypes
  | -- | CR 205.3m: creature types, a changeling adding every one (CR 702.73a).
    CreatureTypes
  | -- | CR 202.3: mana values, a member with no mana cost adding 0 (CR 202.3a).
    ManaValues
  | -- | CR 202.1 / 202.1b: mana costs by symbol, a costless member adding none.
    ManaCosts
  | -- | CR 208.1: powers.
    Powers
  | -- | CR 205.3j / 306.3: planeswalker types, other subtypes adding none.
    PlaneswalkerTypes
  | -- | CR 122.1: kinds of counters, each keyword counter a kind of its own (CR 122.1b).
    CounterKinds
  | -- | CR 201.2b: names, a member with two (a Room with both doors unlocked,
    -- CR 709.5) adding both and a nameless one none.
    Names
  deriving (Bounded, Enum, Eq, Ord, Show)
