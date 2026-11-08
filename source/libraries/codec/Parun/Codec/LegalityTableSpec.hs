module Parun.Codec.LegalityTableSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Codec.LegalityTable as LegalityTable
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Legality as Legality
import qualified Parun.Types.LegalityTable as LegalityTable

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LegalityTable" $ do
  Spec.it s "a card by pool" $
    Common.assertCodec
      s
      LegalityTable.codec
      (LegalityTable.MkLegalityTable (Text.pack "2026-11-07") (Map.singleton (Text.pack "Black Lotus") (Map.fromList [(Text.pack "legacy", Legality.Banned), (Text.pack "vintage", Legality.Restricted)])))
      " {\"date\":\"2026-11-07\",\"cards\":{\"Black Lotus\":{\"legacy\":\"Banned\",\"vintage\":\"Restricted\"}}} "
  -- A card in no pool, such as a plane, is in the table
  -- still: not legal anywhere, which is not unknown.
  Spec.it s "a card no pool has" $
    Common.assertCodec
      s
      LegalityTable.codec
      (LegalityTable.MkLegalityTable (Text.pack "2026-11-07") (Map.singleton (Text.pack "Agyrem") Map.empty))
      " {\"date\":\"2026-11-07\",\"cards\":{\"Agyrem\":{}}} "
