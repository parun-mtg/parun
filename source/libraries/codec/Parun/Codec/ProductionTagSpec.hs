module Parun.Codec.ProductionTagSpec where

import qualified Parun.Codec.ProductionTag as ProductionTag
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ProductionTag as ProductionTag
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ProductionTag" $ do
  Spec.it s "Snow" $
    Common.assertCodec
      s
      ProductionTag.codec
      ProductionTag.Snow
      " \"Snow\" "
  Spec.it s "Subtype" $
    Common.assertCodec
      s
      ProductionTag.codec
      (ProductionTag.Subtype Subtype.Cave)
      " {\"Subtype\":\"Cave\"} "
  Spec.it s "Artifact" $
    Common.assertCodec
      s
      ProductionTag.codec
      ProductionTag.Artifact
      " \"Artifact\" "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ProductionTag.codec
