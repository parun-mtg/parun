module Parun.Types.Tap where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's Tap arm (CR 701.26a).
data Tap = MkTap
  { ref :: ObjectRef.ObjectRef,
    -- | Where the permanents this tap turned sideways are bound, as a group --
    -- Master of the Wild Hunt's "each Wolf tapped this way". Nothing is bound
    -- when nothing was tapped.
    tapped :: Maybe SlotName.SlotName,
    -- | CR 608.2c: Just the player the instruction tells to tap, Power Sink's
    -- "they tap"; Nothing for the resolving spell or ability's controller.
    tapper :: Maybe PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)

-- | Tap what the ObjectRef names, binding nothing, tapped by the controller.
plain :: ObjectRef.ObjectRef -> Tap
plain r = MkTap {ref = r, tapped = Nothing, tapper = Nothing}
