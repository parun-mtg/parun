module Parun.Types.RollExtreme where

import qualified Parun.Types.RollEnd as RollEnd
import qualified Parun.Types.SlotName as SlotName

-- | Parun.Types.RollDie's players at one end of the results, where several
-- rolled (CR 706.1): bound at `slot` as players for a later clause to name.
data RollExtreme = MkRollExtreme
  { end :: RollEnd.RollEnd,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
