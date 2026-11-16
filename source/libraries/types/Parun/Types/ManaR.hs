module Parun.Types.ManaR where

import Numeric.Natural (Natural)
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaInstead as ManaInstead
import qualified Parun.Types.ManaScale as ManaScale

-- | The payload of Parun.Types.ReplacementEffect's ManaR arm: CR 106.12b's
-- permanent "tapped for mana", the type it produces "instead of any other
-- type" (Naked Singularity's "Plains produce {R}", Pulse of Llanowar's "mana
-- of a color of your choice"; Nothing keeps it, Mana Reflection's "that
-- mana"), and the amount (Contamination's "{B} instead of any other type and
-- amount", Mana Reflection's "twice as much"). Ritual of Subdual's ruling
-- states that nothing keeps the amount there. `atLeast` is the least amount
-- the row applies to (Damping Sphere's "tapped for two or more mana");
-- Nothing applies whatever the amount.
data ManaR = MkManaR
  { matching :: Filter.Filter Keyword.Keyword,
    produces :: Maybe ManaInstead.ManaInstead,
    amount :: ManaScale.ManaScale,
    atLeast :: Maybe Natural
  }
  deriving (Eq, Ord, Show)
