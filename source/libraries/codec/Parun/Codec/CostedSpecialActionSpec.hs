module Parun.Codec.CostedSpecialActionSpec where

import qualified Parun.Codec.CostedSpecialAction as CostedSpecialAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CostedSpecialAction as CostedSpecialAction.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CostedSpecialAction" $ do
  Spec.it s "Plot" $
    Common.assertCodec
      s
      CostedSpecialAction.codec
      CostedSpecialAction.Type.Plot
      " \"Plot\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CostedSpecialAction.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostedSpecialAction.codec
