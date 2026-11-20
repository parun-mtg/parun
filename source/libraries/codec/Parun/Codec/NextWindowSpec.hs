module Parun.Codec.NextWindowSpec where

import qualified Parun.Codec.NextWindow as NextWindow
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.NextWindow as NextWindow.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.NextWindow" $ do
  Spec.it s "CombatPhase" $
    Common.assertCodec
      s
      NextWindow.codec
      NextWindow.Type.CombatPhase
      " \"CombatPhase\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s NextWindow.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s NextWindow.codec
