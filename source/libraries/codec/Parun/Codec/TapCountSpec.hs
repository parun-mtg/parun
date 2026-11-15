module Parun.Codec.TapCountSpec where

import qualified Parun.Codec.TapCount as TapCount
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.TapCount as TapCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TapCount" $ do
  -- Interface Ace's "using its toughness rather than its power".
  Spec.it s "UsingToughness" $
    Common.assertCodec
      s
      TapCount.codec
      TapCount.UsingToughness
      " {\"type\":\"UsingToughness\"} "
  -- Deathless Pilot's "as though its power were 2 greater".
  Spec.it s "GreaterBy" $
    Common.assertCodec
      s
      TapCount.codec
      (TapCount.GreaterBy 2)
      " {\"type\":\"GreaterBy\",\"value\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TapCount.codec
