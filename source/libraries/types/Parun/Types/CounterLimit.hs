module Parun.Types.CounterLimit where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Keyword as Keyword

-- | CR 122.4 / 704.5r: "this permanent can't have more than N counters of a
-- kind on it" (Rasputin Dreamweaver). It limits the permanent holding it, so it
-- carries no affected set; Parun.Engine.CounterLimit is its only reader.
data CounterLimit = MkCounterLimit
  { -- | The kind of counter limited (CR 122.1).
    kind :: CounterKind.CounterKind Keyword.Keyword,
    -- | N: the most counters of that kind the permanent keeps.
    atMost :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
