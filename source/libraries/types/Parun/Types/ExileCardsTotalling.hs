module Parun.Types.ExileCardsTotalling where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardTally as CardTally
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Zone as Zone

-- | The payload of Parun.Types.CostComponent's ExileCardsTotalling arm: exile
-- objects matching the Filter from the zone, at least `fewest` of them, whose
-- tally compares to the threshold as the Comparison says. Baron Helmut Zemo's
-- graveyard cards reach it (AtLeast); Fabrication Foundry's permanents total
-- an announced X exactly.
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is.
data ExileCardsTotalling keyword = MkExileCardsTotalling
  { threshold :: CostAmount.CostAmount,
    comparison :: Comparison.Comparison,
    -- | The fewest objects a payment exiles: Fabrication Foundry's "one or
    -- more", which an exact total of 0 does not otherwise state.
    fewest :: Natural.Natural,
    tally :: CardTally.CardTally,
    whichCards :: Filter.Filter keyword,
    -- | The payer's graveyard, or the battlefield (CR 406.2).
    --
    -- Unprinted: any other zone. Parun.Codec.ExileCardsTotalling refuses it.
    zone :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
