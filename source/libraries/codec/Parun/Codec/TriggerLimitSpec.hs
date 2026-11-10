module Parun.Codec.TriggerLimitSpec where

import qualified Parun.Codec.TriggerLimit as TriggerLimit
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.SpentOn as SpentOn
import qualified Parun.Types.TriggerLimit as TriggerLimit

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TriggerLimit" $ do
  Spec.it s "Unlimited" $
    Common.assertCodec
      s
      TriggerLimit.codec
      TriggerLimit.Unlimited
      " {\"type\":\"Unlimited\"} "
  -- "This ability triggers only once each turn": the spender is elided.
  Spec.it s "OncePerTurn spent on triggering" $
    Common.assertCodec
      s
      TriggerLimit.codec
      (TriggerLimit.OncePerTurn SpentOn.Triggering)
      " {\"type\":\"OncePerTurn\"} "
  -- CR 603.2h's "Do this only once each turn".
  Spec.it s "OncePerTurn spent on taking the instruction" $
    Common.assertCodec
      s
      TriggerLimit.codec
      (TriggerLimit.OncePerTurn SpentOn.TakingInstruction)
      " {\"type\":\"OncePerTurn\",\"value\":\"TakingInstruction\"} "
  Spec.it s "OncePerGame" $
    Common.assertCodec
      s
      TriggerLimit.codec
      TriggerLimit.OncePerGame
      " {\"type\":\"OncePerGame\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TriggerLimit.codec
