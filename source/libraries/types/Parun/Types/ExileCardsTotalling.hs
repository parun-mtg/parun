module Parun.Types.ExileCardsTotalling where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.CostComponent's ExileCardsTotalling arm: exile
-- any number of cards matching the Filter from the paying player's graveyard
-- whose tally reaches the threshold.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is.
data ExileCardsTotalling keyword = MkExileCardsTotalling
  { threshold :: Natural.Natural,
    tally :: CardTally.CardTally,
    whichCards :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
