module Parun.Types.BindLeaders where

import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's BindLeaders arm: the slot whose objects
-- are ranked, the measure each is ranked by, and the slot the players owning
-- the greatest are bound under.
data BindLeaders = MkBindLeaders
  { among :: SlotName.SlotName,
    measure :: Measure.Measure,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
