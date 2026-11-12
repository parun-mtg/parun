module Parun.Codec.TotalCapSpec where

import qualified Parun.Codec.TotalCap as TotalCap
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.TotalCap as TotalCap

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TotalCap" $ do
  -- Michelangelo's Technique's "with total mana value 6 or less".
  Spec.it s "MkTotalCap, both keys" $
    Common.assertCodec
      s
      TotalCap.codec
      TotalCap.MkTotalCap {TotalCap.weight = Quantity.Measure Measure.ManaValue, TotalCap.most = Quantity.Literal 6}
      " {\"weight\":{\"type\":\"Measure\",\"value\":\"ManaValue\"},\"most\":{\"type\":\"Literal\",\"value\":6}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TotalCap.codec
