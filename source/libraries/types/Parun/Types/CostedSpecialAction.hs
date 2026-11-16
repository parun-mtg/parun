module Parun.Types.CostedSpecialAction where

-- | Which CR 116.2 special action a Parun.Types.CostSubject.SpecialActions
-- modifier reaches. One constructor per special action a printed cost modifier
-- names.
data CostedSpecialAction
  = -- | CR 116.2m / 709.5e: paying an unlock cost.
    Unlock
  | -- | CR 116.2k / 702.170e: plotting a card.
    Plot
  deriving (Bounded, Enum, Eq, Ord, Show)
