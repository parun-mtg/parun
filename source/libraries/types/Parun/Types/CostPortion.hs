module Parun.Types.CostPortion where

-- | Which part of an object's total cost (CR 601.2f) a reading of the mana
-- spent on it covers (Parun.Types.ManaSpent's OfType).
data CostPortion
  = -- | CR 601.2h: all of it (Parun.Types.Object.manaSpent).
    TotalCost
  | -- | CR 107.3 / 601.2h: its {X} alone (Parun.Types.Object.manaSpentOnX).
    OnX
  deriving (Bounded, Enum, Eq, Ord, Show)
