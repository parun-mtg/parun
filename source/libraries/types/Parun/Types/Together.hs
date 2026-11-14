module Parun.Types.Together where

import qualified Parun.Types.DifferentIn as DifferentIn

-- | CR 601.2c: a restriction a slot's text places on its targets TOGETHER
-- rather than on each one.
data Together
  = -- | Decompose's "from a single graveyard", Misinformation's "from an
    -- opponent's graveyard": every target in one zone (CR 400.1).
    InOneZone
  | -- | Behold the Sinister Six!'s "with different names": no two targets
    -- share a value on the axis (CR 201.2b).
    Differing DifferentIn.DifferentIn
  deriving (Eq, Ord, Show)
