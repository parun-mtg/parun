module Parun.Types.ColorAmong where

import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 105.2 / 106.1a: mana of the colours among the objects a filter matches,
-- in any zone (Parun.Types.ManaProduction.ColorAmong).
data ColorAmong = MkColorAmong
  { among :: Filter.Filter Keyword.Keyword,
    -- | Nothing settles one mana of each colour among them (Bloom Tender); a
    -- spread is CR 105.4's choice among them (Pit of Offerings).
    spread :: Maybe ColorSpread.ColorSpread
  }
  deriving (Eq, Ord, Show)
