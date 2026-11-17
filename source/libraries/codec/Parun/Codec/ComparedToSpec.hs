module Parun.Codec.ComparedToSpec where

import qualified Parun.Codec.ComparedTo as ComparedTo
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ComparedTo as ComparedTo
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ComparedTo" $ do
  Spec.it s "MkComparedTo" $
    Common.assertCodec
      s
      ComparedTo.codec
      (ComparedTo.MkComparedTo Comparison.LessThan (Quantity.Measure Measure.Power))
      " {\"comparison\":\"LessThan\",\"threshold\":{\"type\":\"Measure\",\"value\":\"Power\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ComparedTo.codec
