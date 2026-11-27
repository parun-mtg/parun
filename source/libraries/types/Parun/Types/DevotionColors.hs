module Parun.Types.DevotionColors where

import qualified Data.Set as Set
import qualified Parun.Types.Color as Color

-- | CR 700.5: the colours a devotion counts (Parun.Types.Devotion's `colors`).
data DevotionColors
  = -- | CR 700.5: the colours the card names (Gray Merchant of Asphodel; Mogis, God of Slaughter).
    Listed (Set.Set Color.Color)
  | -- | CR 700.5 / 608.2d: the colours last chosen for the source (Object.chosenColors), none reading 0 (Nyx, Hotel of Fears).
    SourceChosen
  deriving (Eq, Ord, Show)
