module Parun.Types.ControlPlayerNext where

import qualified Parun.Types.ControlSpan as ControlSpan
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's ControlPlayerNext arm (CR 724.1): which
-- slot names the player controlled, and which stretch of their time it waits
-- for.
data ControlPlayerNext = MkControlPlayerNext
  { slot :: SlotName.SlotName,
    during :: ControlSpan.ControlSpan
  }
  deriving (Eq, Ord, Show)
