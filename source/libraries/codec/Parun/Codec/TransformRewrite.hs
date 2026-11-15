module Parun.Codec.TransformRewrite where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.TransformRewrite as TransformRewrite

-- | The effect codec is a PARAMETER, Parun.Codec.AttachRewrite's posture.
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (TransformRewrite.TransformRewrite effect)
codec effectCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "RunEffects" (Common.seq effectCodec) TransformRewrite.RunEffects (\x -> case x of TransformRewrite.RunEffects y -> Just y; _ -> Nothing),
      Arm.payload "ChoosePlayer" PlayerRelation.codec TransformRewrite.ChoosePlayer (\x -> case x of TransformRewrite.ChoosePlayer y -> Just y; _ -> Nothing)
    ]

tagOf :: TransformRewrite.TransformRewrite effect -> String
tagOf x = case x of
  TransformRewrite.RunEffects {} -> "RunEffects"
  TransformRewrite.ChoosePlayer {} -> "ChoosePlayer"
