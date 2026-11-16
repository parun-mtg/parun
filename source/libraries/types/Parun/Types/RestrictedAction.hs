module Parun.Types.RestrictedAction where

import qualified Parun.Types.Designation as Designation

-- | Which game action a printed object prohibition
-- (Parun.Types.ObjectRestriction) forbids (CR 101.2).
data RestrictedAction
  = -- | It can't be sacrificed (CR 701.21a).
    Sacrifice
  | -- | It can't be turned face up (CR 116.2b, 708).
    TurnFaceUp
  | -- | It can't gain the designation (CR 701.60a's "becomes suspected").
    BecomeDesignated Designation.Designation
  deriving (Eq, Ord, Show)
