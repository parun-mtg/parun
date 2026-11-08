module Parun.Types.StateBasedAction where

-- | A state-based action a player's permanents can be exempted from.
data StateBasedAction
  = -- | CR 704.5i: a planeswalker with loyalty 0 is put into its owner's
    -- graveyard.
    ZeroLoyalty
  | -- | CR 704.5j: the legend rule.
    LegendRule
  deriving (Bounded, Enum, Eq, Ord, Show)
