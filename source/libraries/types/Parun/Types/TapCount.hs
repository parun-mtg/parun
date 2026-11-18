module Parun.Types.TapCount where

import qualified Numeric.Natural as Natural

-- | How Parun.Types.Modification's TapsCounting changes what a creature
-- counts toward a keyword that taps it (CR 702.122a, 702.171a, 702.184a).
data TapCount
  = -- | CR 613.11: its toughness rather than its power (Interface Ace).
    UsingToughness
  | -- | CR 613.11: its power as though it were N greater (Deathless Pilot).
    GreaterBy Natural.Natural
  | -- | CR 613.11: its power as though it were N times as great (Sulu, Heroic
    -- Helmsman).
    Multiplied Natural.Natural
  deriving (Eq, Ord, Show)
