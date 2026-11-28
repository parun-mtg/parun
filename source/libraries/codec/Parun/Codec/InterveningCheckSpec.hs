module Parun.Codec.InterveningCheckSpec where

import qualified Parun.Codec.InterveningCheck as InterveningCheck
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.InterveningCheck as InterveningCheck

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.InterveningCheck" $ do
  Spec.it s "OnTriggerAndResolution" $
    Common.assertCodec
      s
      InterveningCheck.codec
      InterveningCheck.OnTriggerAndResolution
      " \"OnTriggerAndResolution\" "
  Spec.it s "OnTriggerOnly" $
    Common.assertCodec
      s
      InterveningCheck.codec
      InterveningCheck.OnTriggerOnly
      " \"OnTriggerOnly\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s InterveningCheck.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s InterveningCheck.codec
