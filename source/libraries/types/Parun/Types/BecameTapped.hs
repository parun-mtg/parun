module Parun.Types.BecameTapped where

import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 701.26a: one permanent became tapped, and the player who tapped it ---
-- the player an effect instructed (CR 608.2c), a cost's payer, the attacking
-- player (CR 508.1f), a regenerating permanent's controller (CR 701.19a) ---
-- which Hylda of the Icy Crown's "whenever you tap" reads.
data BecameTapped = MkBecameTapped
  { permanent :: ObjectId.ObjectId,
    tapper :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
