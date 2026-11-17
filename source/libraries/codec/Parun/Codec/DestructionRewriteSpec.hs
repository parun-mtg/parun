module Parun.Codec.DestructionRewriteSpec where

import qualified Parun.Codec.DestructionRewrite as DestructionRewrite
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.DestructionRewrite as DestructionRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DestructionRewrite" $ do
  Spec.it s "Regenerate" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      DestructionRewrite.Regenerate
      " {\"type\":\"Regenerate\"} "
  -- CR 122.1c's replacement half. Minted from a permanent's shield counters and
  -- never authored on a card, so this codec is the only place its wire form is
  -- pinned.
  Spec.it s "RemoveShieldCounter" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      DestructionRewrite.RemoveShieldCounter
      " {\"type\":\"RemoveShieldCounter\"} "
  -- CR 702.89a. Engine-minted, so pinned here as the shield counter is.
  Spec.it s "UmbraArmor" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      DestructionRewrite.UmbraArmor
      " {\"type\":\"UmbraArmor\"} "
  -- Pyramids' bare heal omits the value.
  Spec.it s "Heal without a counter kind" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      (DestructionRewrite.Heal Nothing)
      " {\"type\":\"Heal\"} "
  -- Dr. McCoy, Surly Surgeon's "remove all +1/+1 counters from it and all damage
  -- already dealt to it is healed".
  Spec.it s "Heal with a counter kind" $
    Common.assertCodec
      s
      DestructionRewrite.codec
      (DestructionRewrite.Heal (Just CounterKind.PlusOnePlusOne))
      " {\"type\":\"Heal\",\"value\":{\"type\":\"PlusOnePlusOne\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s DestructionRewrite.codec
