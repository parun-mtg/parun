module Parun.Codec.PrintingTableSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.PrintingTable as PrintingTable
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PrintingEntry as PrintingEntry
import qualified Parun.Types.PrintingTable as PrintingTable

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PrintingTable" $ do
  -- CR 718.1: only an Attraction writes its lights.
  Spec.it s "a printing by set and number, lights only where lit" $
    Common.assertCodec
      s
      PrintingTable.codec
      ( PrintingTable.MkPrintingTable
          (Text.pack "2026-11-07")
          ( Map.singleton
              (Text.pack "UNF")
              ( Map.fromList
                  [ (Text.pack "202a", PrintingEntry.MkPrintingEntry (Text.pack "Bumper Cars") (Set.fromList [2, 3, 6])),
                    (Text.pack "35", PrintingEntry.MkPrintingEntry (Text.pack "Deadbeat Attendant") Set.empty)
                  ]
              )
          )
      )
      " {\"date\":\"2026-11-07\",\"printings\":{\"UNF\":{\"202a\":{\"card\":\"Bumper Cars\",\"lights\":[2,3,6]},\"35\":{\"card\":\"Deadbeat Attendant\"}}}} "
