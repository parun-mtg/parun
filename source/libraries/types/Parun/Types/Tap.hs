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
    -- | The ONE player the instruction addresses (CR 608.2c), credited with
    -- each tap: @Relative You@ by default, Power Sink's "they tap" the CR
    -- 608.2f loop's slot. A ref naming no single seat taps nothing (CR 101.3).
    -- data/scenarios/power-sink-they-tap-is-not-you-tapping-for-icewrought-sentry.json
    -- proves it.
    tapper :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)

-- | Tap what the ObjectRef names, binding nothing, as the resolving controller.
plain :: ObjectRef.ObjectRef -> Tap
plain r = MkTap {ref = r, tapped = Nothing, tapper = PlayerRef.Relative PlayerRelation.You}
