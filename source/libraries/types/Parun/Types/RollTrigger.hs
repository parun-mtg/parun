module Parun.Types.RollTrigger where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.RollKind as RollKind
import qualified Parun.Types.RollReading as RollReading

-- | CR 706.2: whose die roll, of which kind, came out as which number --
-- Night Shift of the Living Dead's "whenever you roll a 6", "Lifetime" Pass
-- Holder's "whenever you roll to visit your Attractions, if you roll a 6",
-- Critical Hit's "when you roll a natural 20".
data RollTrigger = MkRollTrigger
  { -- | The roller, read against CR 109.5's "you".
    roller :: PlayerRelation.PlayerRelation,
    -- | The number `reading` names.
    result :: Natural.Natural,
    roll :: RollKind.RollKind,
    reading :: RollReading.RollReading
  }
  deriving (Eq, Ord, Show)
