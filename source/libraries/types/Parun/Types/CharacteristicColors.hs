module Parun.Types.CharacteristicColors where

import qualified Data.Set as Set
import qualified Parun.Types.Color as Color

-- | CR 604.3 / 105.2: what a face's colour-defining ability says it is.
data CharacteristicColors = MkCharacteristicColors
  { -- | The colours it names (Transguild Courier's "is all colors").
    colors :: Set.Set Color.Color,
    -- | CR 903.4b / 607.2p: it is also the colour chosen for it before the
    -- game (The Prismatic Piper), read off Printing.chosenColor.
    chosen :: Bool
  }
  deriving (Eq, Ord, Show)

-- | CR 709.4c: a split card has each half's ability, so both halves' colours.
instance Semigroup CharacteristicColors where
  l <> r = MkCharacteristicColors {colors = colors l <> colors r, chosen = chosen l || chosen r}
