module Parun.Types.SubtypeFamily where

-- | CR 205.3: the family a choice of a subtype (Parun.Types.ChosenQuality)
-- picks from, and which family Parun.Types.Modification's ChosenSubtype applies
-- it as.
data SubtypeFamily
  = -- | CR 305.6 / 305.7: a basic land type (Convincing Mirage).
    BasicLandType
  | -- | CR 205.3m: a creature type (Adaptive Automaton).
    CreatureType
  | -- | CR 205.3j: a planeswalker type (Deification).
    PlaneswalkerType
  deriving (Bounded, Enum, Eq, Ord, Show)
