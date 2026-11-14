module Parun.Types.LifeExchange where

import qualified Parun.Types.ExchangeSides as ExchangeSides
import qualified Parun.Types.SlotName as SlotName

-- | The payload of Parun.Types.Effect's ExchangeLifeTotals arm: the two sides,
-- plus a tally, spun out the way Parun.Types.LifeLoss was for the same field.
data LifeExchange = MkLifeExchange
  { sides :: ExchangeSides.ExchangeSides,
    -- | Where to record how much life the CONTROLLER lost in the exchange (CR
    -- 701.12c), for a later effect of the same resolution to read as
    -- Quantity.InSlot: Mister Negative's "if you lost life this way, draw that
    -- many cards". 0 where the controller gained, stayed or is not a side.
    -- Nothing for every other printing.
    tally :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
