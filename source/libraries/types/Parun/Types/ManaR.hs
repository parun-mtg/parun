module Parun.Types.ManaR where

import Numeric.Natural (Natural)
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaType as ManaType

-- | The payload of Parun.Types.ReplacementEffect's ManaR arm: CR 106.12b's
-- permanent "tapped for mana", the type it produces "instead of any other
-- type" (Naked Singularity's "Plains produce {R}"), and the amount where the
-- row replaces that too (Contamination's "{B} instead of any other type and
-- amount"). Nothing keeps the amount, which Ritual of Subdual's ruling states.
data ManaR = MkManaR
  { matching :: Filter.Filter Keyword.Keyword,
    produces :: ManaType.ManaType,
    amount :: Maybe Natural
  }
  deriving (Eq, Ord, Show)
