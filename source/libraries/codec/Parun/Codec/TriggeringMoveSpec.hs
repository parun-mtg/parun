module Parun.Codec.TriggeringMoveSpec where

import qualified Parun.Codec.TriggeringMove as TriggeringMove
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.TriggeringMove as TriggeringMove

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TriggeringMove" $ do
  Spec.it s "Entering" $
    Common.assertCodec
      s
      TriggeringMove.codec
      TriggeringMove.Entering
      " \"Entering\" "
  Spec.it s "Dying" $
    Common.assertCodec
      s
      TriggeringMove.codec
      TriggeringMove.Dying
      " \"Dying\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TriggeringMove.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s TriggeringMove.codec
