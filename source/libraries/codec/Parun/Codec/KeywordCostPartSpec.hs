module Parun.Codec.KeywordCostPartSpec where

import qualified Parun.Codec.KeywordCostPart as KeywordCostPart
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.KeywordCostPart" $ do
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s KeywordCostPart.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s KeywordCostPart.codec
