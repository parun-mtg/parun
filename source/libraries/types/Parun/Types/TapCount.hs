module Parun.Types.TapCount where

import qualified Numeric.Natural as Natural

-- | How Parun.Types.Modification's TapsCounting changes what a creature
-- counts toward a tap-for-total-power cost (CR 702.122a, 702.171a).
data TapCount
  = -- | CR 613.11: its toughness rather than its power (Interface Ace).
    UsingToughness
  | -- | CR 613.11: its power as though it were N greater (Deathless Pilot).
    GreaterBy Natural.Natural
  deriving (Eq, Ord, Show)
