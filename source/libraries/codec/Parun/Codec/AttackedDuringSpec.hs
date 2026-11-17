module Parun.Codec.AttackedDuringSpec where

import qualified Parun.Codec.AttackedDuring as AttackedDuring
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackedDuring as AttackedDuring

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackedDuring" $ do
  Spec.it s "TheirLastTurn" $
    Common.assertCodec
      s
      AttackedDuring.codec
      AttackedDuring.TheirLastTurn
      " \"TheirLastTurn\" "
  Spec.it s "ThisTurn" $
    Common.assertCodec
      s
      AttackedDuring.codec
      AttackedDuring.ThisTurn
      " \"ThisTurn\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s AttackedDuring.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackedDuring.codec
