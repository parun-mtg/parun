module Parun.Types.ManifestDread where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's ManifestDread arm: CR 701.62a performed
-- by these players, this many times each (Valgavoth's Onslaught's "manifest
-- dread X times"), plus the name "those creatures" is read under.
--
-- Parun.Types.Blight's shape, for its reason.
data ManifestDread = MkManifestDread
  { player :: PlayerRef.PlayerRef,
    -- | How many times each player manifests dread, one CR 701.62a action
    -- apiece (CR 701.62b's trigger fires for each).
    quantity :: Quantity.Quantity,
    -- | The permanents the manifests put onto the battlefield, gathered across
    -- every action, for a later effect of the same resolution to name. Absent
    -- for every manifest dread nothing looks back at.
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
