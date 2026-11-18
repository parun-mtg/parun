module Parun.Codec.ControllerGrantSpec where

import qualified Parun.Codec.ControllerGrant as ControllerGrant
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ControllerGrant as ControllerGrant

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ControllerGrant" $ do
  Spec.it s "SpeedBeyondFour" $
    Common.assertCodec
      s
      ControllerGrant.codec
      ControllerGrant.SpeedBeyondFour
      " \"SpeedBeyondFour\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ControllerGrant.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ControllerGrant.codec
