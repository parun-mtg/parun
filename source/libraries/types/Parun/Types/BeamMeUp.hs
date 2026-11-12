module Parun.Types.BeamMeUp where

import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.Keyword's BeamMeUp arm: CR 702.196a's [cost],
-- plus the creatures this instance's return may not pick.
--
-- PARAMETRIC in the keyword, Parun.Types.Protection's shape and for its
-- reason. Only @BeamMeUp Keyword.Keyword@ is ever written.
data BeamMeUp keyword = MkBeamMeUp
  { -- | CR 702.196a's "[cost]", without the return of a creature, which
    -- Parun.Engine.Cost.candidateCostsGiven adds.
    cost :: Cost.Cost keyword,
    -- | CR 702.196b scoped to this instance (Scotty, Miracle Worker), its
    -- Filter.IsSource baked to the granter by Parun.Engine.Keyword.grantedBy.
    -- On the keyword, not a Parun.Types.KeywordCostRestriction, so another
    -- instance is unaffected, which the scenario
    -- scotty-miracle-worker-can-be-returned-for-a-printed-beam-me-up proves.
    unreturnable :: Maybe (Filter.Filter keyword)
  }
  deriving (Eq, Ord, Show)
