module Parun.Codec.AttackWindowSpec where

import qualified Parun.Codec.AttackWindow as AttackWindow
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackWindow as AttackWindow

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackWindow" $ do
  Spec.it s "ThisTurn" $
    Common.assertCodec
      s
      AttackWindow.codec
      AttackWindow.ThisTurn
      " \"ThisTurn\" "
  Spec.it s "ThisCombat" $
    Common.assertCodec
      s
      AttackWindow.codec
      AttackWindow.ThisCombat
      " \"ThisCombat\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s AttackWindow.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackWindow.codec
