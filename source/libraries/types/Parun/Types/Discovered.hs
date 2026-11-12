module Parun.Types.Discovered where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerId as PlayerId

-- | The payload of Parun.Types.GameEvent's Discovered arm: CR 701.57b's
-- finished discover, who discovered and the N it was for.
data Discovered = MkDiscovered
  { player :: PlayerId.PlayerId,
    value :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
