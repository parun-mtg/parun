module Parun.Types.Tap where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's Tap arm (CR 701.26a).
data Tap = MkTap
  { ref :: ObjectRef.ObjectRef,
    -- | Where the permanents this tap turned sideways are bound, as a group --
    -- Master of the Wild Hunt's "each Wolf tapped this way". Nothing is bound
    -- when nothing was tapped.
    tapped :: Maybe SlotName.SlotName,
    -- | The player the instruction tells to tap (CR 608.2c): Power Sink's
    -- "they tap", the countered spell's controller. ONE seat, Divide's
    -- chooser's reading; a ref naming no seat taps nothing (CR 101.3).
    tapper :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)

-- | Tap what the ObjectRef names, binding nothing, the resolving controller
-- tapping.
plain :: ObjectRef.ObjectRef -> Tap
plain r = MkTap {ref = r, tapped = Nothing, tapper = PlayerRef.Relative PlayerRelation.You}
