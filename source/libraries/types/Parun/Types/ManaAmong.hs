module Parun.Types.ManaAmong where

import qualified Data.Set as Set
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.ManaType as ManaType

-- | CR 105.4 / 106.1a: mana of types among those printed, chosen as the
-- instruction is carried out and spread as given -- Domri, Chaos Bringer's
-- "{R} or {G}" one choice, Vivi Ornitier's "any combination of {U} and/or {R}"
-- one per unit.
data ManaAmong = MkManaAmong
  { types :: Set.Set ManaType.ManaType,
    spread :: ColorSpread.ColorSpread
  }
  deriving (Eq, Ord, Show)
