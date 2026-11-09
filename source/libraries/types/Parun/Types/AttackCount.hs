module Parun.Types.AttackCount where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.AttackMeasure as AttackMeasure
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 508.3c: "attacks with [n or more creatures]" -- the declared creatures
-- the Filter admits, measured, reach at least this floor (Military
-- Intelligence's two creatures, Desert Were-Worm's total power 12).
data AttackCount = MkAttackCount
  { measure :: AttackMeasure.AttackMeasure,
    floor :: Natural.Natural,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
