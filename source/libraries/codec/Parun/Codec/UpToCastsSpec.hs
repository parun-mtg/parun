module Parun.Codec.UpToCastsSpec where

import qualified Parun.Codec.UpToCasts as UpToCasts
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TotalCap as TotalCap
import qualified Parun.Types.UpToCasts as UpToCasts

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.UpToCasts" $ do
  Spec.it s "omits a missing total" $
    Common.assertCodec
      s
      UpToCasts.codec
      UpToCasts.MkUpToCasts {UpToCasts.casts = Quantity.Literal 2, UpToCasts.total = Nothing}
      " {\"casts\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "carries a total" $
    Common.assertCodec
      s
      UpToCasts.codec
      UpToCasts.MkUpToCasts {UpToCasts.casts = Quantity.Literal 2, UpToCasts.total = Just TotalCap.MkTotalCap {TotalCap.weight = Quantity.Measure Measure.ManaValue, TotalCap.most = Quantity.Literal 6}}
      " {\"casts\":{\"type\":\"Literal\",\"value\":2},\"total\":{\"most\":{\"type\":\"Literal\",\"value\":6},\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s UpToCasts.codec
