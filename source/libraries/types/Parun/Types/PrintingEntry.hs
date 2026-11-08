module Parun.Types.PrintingEntry where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural

-- | What one set code and collector number print, as data/printings.json
-- holds it.
data PrintingEntry = MkPrintingEntry
  { -- | Its card's faces' names joined with " // ", Parun.Ingest.cardKey's
    -- key.
    card :: Text.Text,
    -- | CR 718.1: the numbers it lights, empty for every card but an
    -- Attraction.
    lights :: Set.Set Natural.Natural
  }
  deriving (Eq, Show)
