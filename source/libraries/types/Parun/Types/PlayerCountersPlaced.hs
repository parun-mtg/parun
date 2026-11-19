module Parun.Types.PlayerCountersPlaced where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 122.1: a player put counters of one kind on a player, with that
-- player's count BEFORE and AFTER. Parun.Types.CountersPlaced's player half,
-- holding no snapshot since a player is not an object (CR 109.1).
data PlayerCountersPlaced = MkPlayerCountersPlaced
  { putter :: PlayerId.PlayerId,
    player :: PlayerId.PlayerId,
    kind :: PlayerCounterKind.PlayerCounterKind,
    before :: Natural.Natural,
    after :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
