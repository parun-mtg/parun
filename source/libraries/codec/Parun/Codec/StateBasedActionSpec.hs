module Parun.Codec.StateBasedActionSpec where

import qualified Parun.Codec.StateBasedAction as StateBasedAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.StateBasedAction as StateBasedAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.StateBasedAction" $ do
  Spec.it s "ZeroLoyalty" $
    Common.assertCodec
      s
      StateBasedAction.codec
      StateBasedAction.ZeroLoyalty
      " \"ZeroLoyalty\" "
  Spec.it s "LegendRule" $
    Common.assertCodec
      s
      StateBasedAction.codec
      StateBasedAction.LegendRule
      " \"LegendRule\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s StateBasedAction.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s StateBasedAction.codec
