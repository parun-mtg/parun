module Parun.Codec.CostedSpecialActionSpec where

import qualified Parun.Codec.CostedSpecialAction as CostedSpecialAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CostedSpecialAction as CostedSpecialAction.Type
import qualified Parun.Types.TurnUpProcedure as TurnUpProcedure

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CostedSpecialAction" $ do
  Spec.it s "Unlock" $
    Common.assertCodec
      s
      CostedSpecialAction.codec
      CostedSpecialAction.Type.Unlock
      " {\"type\":\"Unlock\"} "
  Spec.it s "Plot" $
    Common.assertCodec
      s
      CostedSpecialAction.codec
      CostedSpecialAction.Type.Plot
      " {\"type\":\"Plot\"} "
  Spec.it s "Foretell" $
    Common.assertCodec
      s
      CostedSpecialAction.codec
      CostedSpecialAction.Type.Foretell
      " {\"type\":\"Foretell\"} "
  Spec.it s "TurnFaceUp" $
    Common.assertCodec
      s
      CostedSpecialAction.codec
      (CostedSpecialAction.Type.TurnFaceUp TurnUpProcedure.Disguise)
      " {\"type\":\"TurnFaceUp\",\"value\":\"Disguise\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CostedSpecialAction.codec
