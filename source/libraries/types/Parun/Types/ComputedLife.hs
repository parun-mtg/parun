module Parun.Types.ComputedLife where

import qualified Parun.Types.Rounding as Rounding

-- | The payload of Parun.Types.CostComponent's PayComputedLife arm: a life
-- payment whose amount the game state fixes as the cost is paid (CR 601.2f),
-- or leaves undefined, which makes the cost unpayable.
data ComputedLife
  = -- | CR 119.4 / 107.1a: half the paying player's life, rounded as printed
    -- (Lurking Evil).
    HalfLife Rounding.Rounding
  | -- | CR 903.4 / 702.124c: the number of colours in the paying player's
    -- commanders' colour identity, undefined without one (CR 903.4f; War Room).
    CommanderColorCount
  deriving (Eq, Ord, Show)
