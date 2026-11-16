module Parun.Types.Measure where

-- | A numeric characteristic read off an object's projected view
-- (Parun.Engine.Filter.measureOf), by a Parun.Types.Filter comparison or a
-- Parun.Types.Quantity.
data Measure
  = -- | CR 208.1: power.
    Power
  | -- | CR 208.1: toughness.
    Toughness
  | -- | CR 208.1: power plus toughness, where the object has both.
    TotalPowerAndToughness
  | -- | CR 208.4b: power through layer 7b, before what modifies it.
    BasePower
  | -- | CR 208.4b: toughness through layer 7b, before what modifies it.
    BaseToughness
  | -- | CR 202.3: mana value.
    ManaValue
  | -- | CR 105.2: how many of the five colours the object is.
    ColorCount
  | -- | CR 205.3m: how many creature types the object has.
    CreatureTypeCount
  | -- | CR 306.5c: a planeswalker's loyalty, its loyalty counters.
    Loyalty
  | -- | CR 709.5c / 709.5j: how many unlocked doors a permanent has.
    UnlockedDoors
  | -- | CR 709.5c / 709.5j: how many locked doors a permanent has.
    LockedDoors
  | -- | CR 700.2d / 700.2e: how many times its controller chose a mode for a
    -- modal spell, a repeated mode counting each time.
    ModesChosen
  deriving (Bounded, Enum, Eq, Ord, Show)
