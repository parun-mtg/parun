module Parun.Codec.DevotionColorsSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.DevotionColors as DevotionColors
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.DevotionColors as DevotionColors

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DevotionColors" $ do
  Spec.it s "Listed" $
    Common.assertCodec
      s
      DevotionColors.codec
      (DevotionColors.Listed (Set.fromList [Color.White, Color.Black]))
      " {\"type\":\"Listed\",\"value\":[\"White\",\"Black\"]} "
  Spec.it s "SourceChosen" $
    Common.assertCodec
      s
      DevotionColors.codec
      DevotionColors.SourceChosen
      " {\"type\":\"SourceChosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DevotionColors.codec
