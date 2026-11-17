module Parun.Types.ChoiceOrdinal where

-- | CR 607.2d: which of an ability's ordered choices of one quality a reader
-- names, or a choice writes ("the first chosen type", "the second chosen
-- type"). A card making one choice reads it as the First.
data ChoiceOrdinal
  = -- | CR 607.2d: the first choice, and a card's only one (Convincing Mirage, Illusionary Terrain).
    First
  | -- | CR 607.2d: the second choice (Illusionary Terrain, Vision Charm).
    Second
  deriving (Bounded, Enum, Eq, Ord, Show)
