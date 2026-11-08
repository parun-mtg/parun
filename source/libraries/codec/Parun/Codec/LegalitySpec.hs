module Parun.Codec.LegalitySpec where

import qualified Parun.Codec.Legality as Legality
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Legality as Legality

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Legality" $ do
  -- MTGJSON's own spellings, so data/legalities.json copies them unchanged.
  Spec.it s "Legal" $
    Common.assertCodec s Legality.codec Legality.Legal " \"Legal\" "
  Spec.it s "Restricted" $
    Common.assertCodec s Legality.codec Legality.Restricted " \"Restricted\" "
  Spec.it s "Banned" $
    Common.assertCodec s Legality.codec Legality.Banned " \"Banned\" "
