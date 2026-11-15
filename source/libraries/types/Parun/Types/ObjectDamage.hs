module Parun.Types.ObjectDamage where

import qualified Parun.Types.DamageDirection as DamageDirection
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's DamageDealtThisTurn arm.
data ObjectDamage = MkObjectDamage
  { -- | Which side of the damage the object is on (CR 120.1).
    direction :: DamageDirection.DamageDirection,
    -- | CR 120.1 / 608.2h: only damage from sources this player controlled as
    -- it was dealt (Grothama, All-Devouring), or any damage when Nothing.
    controlledBy :: Maybe PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
