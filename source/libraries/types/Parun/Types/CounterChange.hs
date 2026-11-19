module Parun.Types.CounterChange where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot

-- | CR 122: counters of one kind left an object, with the count BEFORE and the
-- count AFTER. `snapshot` is the object as they left, which CR 608.2i's "an oil
-- counter was removed from a permanent you controlled this turn" asks.

-- The pair rather than the amount, because CR 310.12b's Siege ability asks
-- whether the LAST counter left, which the amount alone can't answer.
--
-- BOTH counts are a Natural, which is why they are named: a swap would invert
-- every read of the last counter.
data CounterChange = MkCounterChange
  { object :: ObjectId.ObjectId,
    kind :: CounterKind.CounterKind Keyword.Keyword,
    before :: Natural.Natural,
    after :: Natural.Natural,
    snapshot :: ObjectSnapshot.ObjectSnapshot
  }
  deriving (Eq, Ord, Show)
