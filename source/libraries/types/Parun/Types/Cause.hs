module Parun.Types.Cause where

import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 609.1: the spell or ability whose effect made an event, as
-- Parun.Types.Moved and Parun.Types.Discarded record it. Strict, so an event
-- in the log holds no game state.
data Cause = MkCause
  { -- | CR 609.1: its controller as the effect was applied.
    controller :: !PlayerId.PlayerId,
    -- | CR 113.7: its source.
    source :: !ObjectId.ObjectId,
    -- | CR 702.72c: the family of the source's keyword that represents it,
    -- for a triggered ability a keyword of the source mints; Nothing for
    -- every other spell or ability.
    keyword :: !(Maybe KeywordFamily.KeywordFamily)
  }
  deriving (Eq, Ord, Show)
