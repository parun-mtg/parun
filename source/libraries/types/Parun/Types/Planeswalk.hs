module Parun.Types.Planeswalk where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway

-- | The payload of Parun.Types.Effect's Planeswalk arm (CR 701.31).
data Planeswalk = MkPlaneswalk
  { -- | The cards still in the planar deck to planeswalk to, at once (Spatial
    -- Merging), or Nothing for its top card.
    to :: Maybe ObjectRef.ObjectRef,
    away :: PlaneswalkAway.PlaneswalkAway
  }
  deriving (Eq, Ord, Show)
