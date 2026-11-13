module Parun.Types.LentKeyword where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Keyword as Keyword

-- | CR 702.1 / 613.1f: a keyword an object has only through the activated or
-- triggered abilities of it a card lends (Parun.Types.CardAbilities), so it
-- has the keyword (CR 702.1d) and none of the keyword's other abilities.
data LentKeyword = MkLentKeyword
  { keyword :: Keyword.Keyword,
    -- | CR 113.2c: how many times the lending card has it.
    instances :: Natural.Natural,
    -- | The name the lending card's unnamed abilities carry
    -- (Parun.Engine.Binding.acquiredFrom).
    lender :: AbilityName.AbilityName,
    -- | CR 603.1: whether the keyword's triggered abilities are lent too.
    triggered :: Bool,
    -- | The restrictions each lent activated ability gains.
    restrictions :: [ActivationRestriction.ActivationRestriction]
  }
  deriving (Eq, Ord, Show)
