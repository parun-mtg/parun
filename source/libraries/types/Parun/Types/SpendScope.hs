module Parun.Types.SpendScope where

import qualified Parun.Types.ActivationScope as ActivationScope
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | The payments one Parun.Types.SpendManaAsThough clause speaks about (CR
-- 609.4b).
data SpendScope
  = -- | Every payment the player makes (Celestial Dawn).
    EveryPayment
  | -- | CR 601.2h: casting a spell matching the filter (Vizier of the Menagerie).
    Casting (Filter.Filter Keyword.Keyword)
  | -- | CR 602.2b: activating an ability the scope names (Agatha's Soul
    -- Cauldron).
    Activating ActivationScope.ActivationScope
  deriving (Eq, Ord, Show)
