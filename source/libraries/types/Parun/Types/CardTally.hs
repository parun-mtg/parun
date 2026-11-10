module Parun.Types.CardTally where

import qualified Parun.Types.Color as Color

-- | What a threshold cost totals over the cards it exiles
-- (Parun.Types.ExileCardsTotalling).
data CardTally
  = -- | CR 202.3: each card's mana value.
    ManaValue
  | -- | CR 202.1 / 107.4: the mana symbols of this colour in each card's mana
    -- cost, a hybrid symbol counting once (Baron Helmut Zemo).
    ManaSymbolsOf Color.Color
  deriving (Eq, Ord, Show)
