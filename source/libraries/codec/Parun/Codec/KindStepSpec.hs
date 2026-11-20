module Parun.Codec.KindStepSpec where

import qualified Parun.Codec.KindStep as KindStep
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.KindStep as KindStep

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.KindStep" $ do
  Spec.it s "AnotherOrRemoveOne" $
    Common.assertCodec
      s
      KindStep.codec
      KindStep.AnotherOrRemoveOne
      " \"AnotherOrRemoveOne\" "

  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s KindStep.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s KindStep.codec
