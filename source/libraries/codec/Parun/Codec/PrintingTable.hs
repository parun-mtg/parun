{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PrintingTable where

import qualified Parun.Codec.PrintingEntry as PrintingEntry
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PrintingTable as PrintingTable

-- | A bare object keyed by the record's field names, data/printings.json.
codec :: Codec.Codec PrintingTable.PrintingTable
codec = Fields.object $ do
  date <- Fields.required "date" Common.text PrintingTable.date
  printings <- Fields.required "printings" (Common.textMap id Right (Common.textMap id Right PrintingEntry.codec)) PrintingTable.printings
  pure PrintingTable.MkPrintingTable {PrintingTable.date = date, PrintingTable.printings = printings}
