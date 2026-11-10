module Parun.Types.UntapScope where

-- | Which untaps a stored untap prohibition (Parun.Types.Prohibition's Untap)
-- forbids.
data UntapScope
  = -- | Only the untap during its controller's untap step (CR 502.3).
    UntapStep
  | -- | Every untap, by any means (CR 101.2, 701.26b).
    AnyUntap
  deriving (Bounded, Enum, Eq, Ord, Show)
