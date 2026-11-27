module Parun.Codec.QualityChooserSpec where

import qualified Parun.Codec.QualityChooser as QualityChooser
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChoiceDistinct as ChoiceDistinct
import qualified Parun.Types.QualityChooser as QualityChooser

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.QualityChooser" $ do
  Spec.it s "Controller" $
    Common.assertCodec s QualityChooser.codec QualityChooser.Controller " {\"type\":\"Controller\"} "
  Spec.it s "EachPlayer" $
    Common.assertCodec s QualityChooser.codec (QualityChooser.EachPlayer ChoiceDistinct.Distinct) " {\"type\":\"EachPlayer\",\"value\":\"Distinct\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s QualityChooser.codec
