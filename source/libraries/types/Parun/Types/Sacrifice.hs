module Parun.Types.Sacrifice where

import qualified Data.Set as Set
import qualified Parun.Types.DifferentIn as DifferentIn
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ObjectCount as ObjectCount

-- | The payload of Parun.Types.CostComponent's Sacrifice arm (#1305): CR 701.21a
-- as a cost, sacrificing this many matching permanents, or every one.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is.
--
-- A record of its OWN rather than one shared with
-- Parun.Types.TapForTotalPower: that constructor's number is a THRESHOLD on an
-- aggregate and this one's is HOW MANY objects, matched exactly, which is the
-- distinction the CostComponent arm spends a paragraph drawing.
data Sacrifice keyword = MkSacrifice
  { count :: ObjectCount.ObjectCount,
    whichPermanents :: Filter.Filter keyword,
    -- | What no two sacrificed permanents may share -- Transmutation Font's
    -- "three artifact tokens with different names" (CR 201.2b). A joint
    -- condition on the chosen set, which no per-candidate Filter can state.
    differentIn :: Set.Set DifferentIn.DifferentIn
  }
  deriving (Eq, Ord, Show)
