module Parun.Types.ControlSpan where

-- | Which stretch of the controlled player's time a scheduled control covers:
-- the axis Parun.Types.ControlPlayerNext is parameterized on.
data ControlSpan
  = -- | CR 724.1: the next turn that player takes (Mindslaver).
    NextTurn
  | -- | The next combat phase that player takes (Secret of Bloodbending).
    NextCombatPhase
  deriving (Bounded, Enum, Eq, Ord, Show)
