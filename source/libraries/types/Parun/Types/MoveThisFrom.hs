module Parun.Types.MoveThisFrom where

import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.CostComponent's MoveThisFrom arm: the zone the
-- card the cost is on leaves, and where it goes.
data MoveThisFrom = MkMoveThisFrom
  { -- | CR 113.6m: its owner's graveyard or hand, the zone the ability then
    -- functions in, read by Parun.Engine.AbilityZone.costZones.
    zone :: Zone.Zone,
    -- | CR 406.2 / Loxodon Surveyor: exile; CR 400.3 / Daniels, Temporal
    -- Agent: the bottom of its owner's library.
    destination :: GraveyardCostDestination.GraveyardCostDestination
  }
  deriving (Eq, Ord, Show)
