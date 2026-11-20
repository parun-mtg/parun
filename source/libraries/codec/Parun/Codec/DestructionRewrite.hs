module Parun.Codec.DestructionRewrite where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.DestructionRewrite as DestructionRewrite

-- | The effect codec is a PARAMETER, Parun.Codec.DamageRewrite's reason.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (DestructionRewrite.DestructionRewrite effect)
codec effectCodec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Regenerate" DestructionRewrite.Regenerate,
      Arm.nullary "RemoveShieldCounter" DestructionRewrite.RemoveShieldCounter,
      Arm.nullary "UmbraArmor" DestructionRewrite.UmbraArmor,
      Arm.optionalPayload "Heal" (CounterKind.codec Keyword.codec) DestructionRewrite.Heal (\x -> case x of DestructionRewrite.Heal y -> Just y; _ -> Nothing),
      Arm.payload "RunEffects" (Common.seq effectCodec) DestructionRewrite.RunEffects (\x -> case x of DestructionRewrite.RunEffects y -> Just y; _ -> Nothing)
    ]

tagOf :: DestructionRewrite.DestructionRewrite effect -> String
tagOf x = case x of
  DestructionRewrite.Regenerate -> "Regenerate"
  DestructionRewrite.RemoveShieldCounter -> "RemoveShieldCounter"
  DestructionRewrite.UmbraArmor -> "UmbraArmor"
  DestructionRewrite.Heal {} -> "Heal"
  DestructionRewrite.RunEffects {} -> "RunEffects"
