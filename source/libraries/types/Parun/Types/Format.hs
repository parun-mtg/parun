module Parun.Types.Format where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Types.DraftBooster as DraftBooster
import qualified Parun.Types.RuleFamily as RuleFamily

-- | A format: construction rules plus the card pool a deck draws from, one file
-- under data/formats each.
data Format = MkFormat
  { -- | CR 100.2: the construction rules.
    rules :: RuleFamily.RuleFamily,
    -- | The key of the format's pool in Parun.Types.LegalityTable, or no pool
    -- at all: any card, under the rules alone.
    pool :: Maybe Text.Text,
    -- | CR 903.13e / 903.13f: the draft boosters a Commander Draft held; empty
    -- for every other format.
    boosters :: Set.Set DraftBooster.DraftBooster
  }
  deriving (Eq, Ord, Show)
