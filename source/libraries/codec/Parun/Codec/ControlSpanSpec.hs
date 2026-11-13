module Parun.Codec.ControlSpanSpec where

import qualified Parun.Codec.ControlSpan as ControlSpan
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ControlSpan as ControlSpan

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ControlSpan" $ do
  Spec.it s "NextCombatPhase" $
    Common.assertCodec
      s
      ControlSpan.codec
      ControlSpan.NextCombatPhase
      " \"NextCombatPhase\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ControlSpan.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ControlSpan.codec
