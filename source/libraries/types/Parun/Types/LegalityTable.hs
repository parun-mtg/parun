module Parun.Types.LegalityTable where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Types.Legality as Legality

-- | MTGJSON's legalities for the pool's cards, as data/legalities.json holds
-- them: a card's entry names only the pools that have it.
data LegalityTable = MkLegalityTable
  { -- | The day MTGJSON's data was built.
    date :: Text.Text,
    -- | By card, its faces' names joined with " // " as MTGJSON keys it, then
    -- by pool.
    cards :: Map.Map Text.Text (Map.Map Text.Text Legality.Legality)
  }
  deriving (Eq, Show)
