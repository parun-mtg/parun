module Parun.Codec.TransformRewriteSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.TransformRewrite as TransformRewrite
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TransformRewrite as TransformRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TransformRewrite" $ do
  Spec.it s "RunEffects (Sephiroth, One-Winged Angel)" $
    Common.assertCodec s (TransformRewrite.codec Common.text) (TransformRewrite.RunEffects (Seq.singleton (Text.pack "emblem"))) " {\"type\":\"RunEffects\",\"value\":[\"emblem\"]} "
  Spec.it s "ChoosePlayer (Shinryu, Transcendent Rival)" $
    Common.assertCodec s (TransformRewrite.codec Common.text) (TransformRewrite.ChoosePlayer PlayerRelation.Opponent) " {\"type\":\"ChoosePlayer\",\"value\":\"Opponent\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (TransformRewrite.codec Common.text)
