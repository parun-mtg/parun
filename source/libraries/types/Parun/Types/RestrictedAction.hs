module Parun.Types.RestrictedAction where

-- | Which game action a printed object prohibition
-- (Parun.Types.ObjectRestriction) forbids (CR 101.2).
data RestrictedAction
  = -- | It can't be sacrificed (CR 701.21a).
    Sacrifice
  | -- | It can't be turned face up (CR 116.2b, 708).
    TurnFaceUp
  deriving (Bounded, Enum, Eq, Ord, Show)
