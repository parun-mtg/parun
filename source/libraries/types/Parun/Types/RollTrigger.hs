module Parun.Types.RollTrigger where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.RollKind as RollKind

-- | CR 706.2: whose die roll, of which kind, came out as which result --
-- Night Shift of the Living Dead's "whenever you roll a 6", "Lifetime" Pass
-- Holder's "whenever you roll to visit your Attractions, if you roll a 6".
data RollTrigger = MkRollTrigger
  { -- | The roller, read against CR 109.5's "you".
    roller :: PlayerRelation.PlayerRelation,
    -- | The result after every modifier.
    result :: Natural.Natural,
    roll :: RollKind.RollKind
  }
  deriving (Eq, Ord, Show)
