module Parun.Types.RecipientRef where

import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The players or the permanents a resolving instruction names, by the refs a
-- resolution reads: CR 508.1b's requirement to attack them
-- (Parun.Types.RequireAttack), or CR 115.7b's target to change to
-- (Parun.Types.ChangeTargets).
data RecipientRef
  = -- | CR 115.1: the players a ref names (Alluring Siren's "attacks you",
    -- Ricochet's "the player with the lowest result").
    Players PlayerRef.PlayerRef
  | -- | CR 115.1: the permanents a ref names (Gideon Jura's "attack Gideon
    -- Jura", Mizzium Meddler's "to this creature").
    Permanents ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)
