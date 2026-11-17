module Parun.Codec.DestructionRewrite where

import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.DestructionRewrite as DestructionRewrite

codec :: Codec.Codec DestructionRewrite.DestructionRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Regenerate" DestructionRewrite.Regenerate,
      Arm.nullary "RemoveShieldCounter" DestructionRewrite.RemoveShieldCounter,
      Arm.nullary "UmbraArmor" DestructionRewrite.UmbraArmor,
      Arm.optionalPayload "Heal" (CounterKind.codec Keyword.codec) DestructionRewrite.Heal (\x -> case x of DestructionRewrite.Heal y -> Just y; _ -> Nothing)
    ]

tagOf :: DestructionRewrite.DestructionRewrite -> String
tagOf x = case x of
  DestructionRewrite.Regenerate -> "Regenerate"
  DestructionRewrite.RemoveShieldCounter -> "RemoveShieldCounter"
  DestructionRewrite.UmbraArmor -> "UmbraArmor"
  DestructionRewrite.Heal {} -> "Heal"
