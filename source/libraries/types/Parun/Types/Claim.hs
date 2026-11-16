module Parun.Types.Claim where

import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Types.ClaimAxis as ClaimAxis
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Selection as Selection

-- | What one payment SPENDS out of a pool of objects: which resource it draws on
-- (Parun.Types.ClaimAxis), which objects are in that pool for it, and how many of
-- them it takes. CR 118.3's "fully" and CR 118.10 make this worth naming --
-- two payments cannot both have the one object, whether they are two components
-- of one cost (Parun.Engine.Cost.jointlyPayable) or the costs of two mana
-- abilities the same payment activates (Parun.Engine.Mana.payableResolutionsGiven).
--
-- A POOL and a COUNT rather than the objects themselves, because which object a
-- payment will take is not decided until it is paid. Without a selection every
-- object in the pool serves the claim equally, so what it contends for is the
-- pool's SIZE; with one, only the groups it admits serve it.
--
-- The AXIS is what two claims must share to contend at all: a sacrifice and a
-- tapping may both name one creature and both be paid, so they are not two claims
-- on one pool.
data Claim = MkClaim
  { axis :: ClaimAxis.ClaimAxis,
    pool :: Set.Set ObjectId.ObjectId,
    -- | How many objects the claim takes, or with a selection how many disjoint
    -- selections of them.
    count :: Natural,
    -- | What each selection's objects must be together, where not every group
    -- of them serves the claim.
    selection :: Maybe Selection.Selection
  }
  deriving (Eq, Ord, Show)
