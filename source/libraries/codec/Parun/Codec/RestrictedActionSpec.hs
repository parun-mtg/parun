module Parun.Codec.RestrictedActionSpec where

import qualified Parun.Codec.RestrictedAction as RestrictedAction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.RestrictedAction as RestrictedAction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RestrictedAction" $ do
  Spec.it s "Sacrifice" $
    Common.assertCodec
      s
      RestrictedAction.codec
      RestrictedAction.Sacrifice
      " {\"type\":\"Sacrifice\"} "
  Spec.it s "TurnFaceUp" $
    Common.assertCodec
      s
      RestrictedAction.codec
      RestrictedAction.TurnFaceUp
      " {\"type\":\"TurnFaceUp\"} "
  Spec.it s "PhaseIn" $
    Common.assertCodec
      s
      RestrictedAction.codec
      RestrictedAction.PhaseIn
      " {\"type\":\"PhaseIn\"} "
  Spec.it s "BecomeDesignated" $
    Common.assertCodec
      s
      RestrictedAction.codec
      (RestrictedAction.BecomeDesignated Designation.Suspected)
      " {\"type\":\"BecomeDesignated\",\"value\":\"Suspected\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s RestrictedAction.codec
