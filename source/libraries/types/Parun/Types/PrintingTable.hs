module Parun.Types.PrintingTable where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Types.PrintingEntry as PrintingEntry

-- | MTGJSON's printings of the pool's cards, as data/printings.json holds
-- them.
data PrintingTable = MkPrintingTable
  { -- | The day MTGJSON's data was built.
    date :: Text.Text,
    -- | By set code, as MTGJSON writes it (upper case), then by collector
    -- number.
    printings :: Map.Map Text.Text (Map.Map Text.Text PrintingEntry.PrintingEntry)
  }
  deriving (Eq, Show)

-- | A table with no printing, which names none.
empty :: PrintingTable
empty = MkPrintingTable {date = Text.empty, printings = Map.empty}
