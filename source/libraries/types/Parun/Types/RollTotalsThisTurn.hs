module Parun.Types.RollTotalsThisTurn where

import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Quantity's RollTotalsThisTurn arm: whose rolls,
-- and the total they are compared against. Parametric in the quantity for
-- Parun.Types.AgainstSlot's reason.
data RollTotalsThisTurn quantity = MkRollTotalsThisTurn
  { -- | CR 706.1: the player who rolled.
    roller :: PlayerRef.PlayerRef,
    -- | The total an earlier roll's must equal.
    total :: quantity
  }
  deriving (Eq, Ord, Show)
