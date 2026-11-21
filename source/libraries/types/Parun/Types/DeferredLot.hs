module Parun.Types.DeferredLot where

import qualified Parun.Types.AttackChoice as AttackChoice
import qualified Parun.Types.BlockChoice as BlockChoice
import qualified Parun.Types.ObjectId as ObjectId

-- | CR 616.1 / 608.2f: one lot of tokens a creation minted inside
-- Parun.Engine.Event.enteringAtOnce, waiting to enter with the action's other
-- creations, and the CR 508.4 / 509.4 riders putting it into combat once it
-- has.
data DeferredLot = MkDeferredLot
  { -- | The lot's tokens, in mint order.
    tokens :: ![ObjectId.ObjectId],
    -- | CR 508.4: what the creation leaves open about what they attack.
    attacking :: !(Maybe AttackChoice.AttackChoice),
    -- | CR 509.4: what the creation leaves open about what they block.
    blocking :: !(Maybe BlockChoice.BlockChoice)
  }
  deriving (Eq, Ord, Show)
