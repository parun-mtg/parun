module Parun.Types.SacrificeCause where

-- | Why a permanent was sacrificed, as far as a rule distinguishes it: CR
-- 702.82b's "devoured" from every other CR 701.21a sacrifice.
data SacrificeCause
  = -- | CR 701.21a: any sacrifice CR 702.82b does not call devouring.
    Ordinary
  | -- | CR 702.82b: sacrificed as a result of a devour ability as its permanent entered.
    Devour
  deriving (Bounded, Enum, Eq, Ord, Show)
