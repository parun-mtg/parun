module Parun.Types.CounterLossR where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | The payload of Parun.Types.ReplacementEffect's CounterLossR arm: which
-- permanents' counters of which kind it watches damage remove, and how many it
-- leaves on them.
data CounterLossR = MkCounterLossR
  { -- | The permanent losing the counters, matched as it is when the damage is
    -- processed (CR 120.4c).
    onWhat :: Filter.Filter Keyword.Keyword,
    whichKind :: CounterKind.CounterKind Keyword.Keyword,
    -- | CR 614.1a: the count the removal stops at (Deification's "all but
    -- one"), and the row applies only to a removal that would go below it.
    leaveAtLeast :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
