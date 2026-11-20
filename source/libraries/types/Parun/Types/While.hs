module Parun.Types.While where

import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 611.2b's "for as long as" expiry: the condition to re-check, and the
-- player it is checked as.

-- The PlayerId is CR 109.5's "you", BAKED at arming time rather than re-derived:
-- the effect outlives the resolution that made it, so the seat the condition
-- reads has to travel with it.
data While = MkWhile
  { player :: PlayerId.PlayerId,
    condition :: Condition.Condition,
    -- | CR 611.2b: the resolution whose later instructions may yet begin this
    -- duration, which had not begun when it was armed; Nothing once it has.
    unbegun :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
