module Parun.Codec.RestrictedActionSpec where

import qualified Parun.Codec.RestrictedAction as RestrictedAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RestrictedAction as RestrictedAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RestrictedAction" $ do
  Spec.it s "TurnFaceUp" $
    Common.assertCodec
      s
      RestrictedAction.codec
      RestrictedAction.TurnFaceUp
      " \"TurnFaceUp\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RestrictedAction.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RestrictedAction.codec
