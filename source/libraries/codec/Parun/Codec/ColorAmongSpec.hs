module Parun.Codec.ColorAmongSpec where

import qualified Parun.Codec.ColorAmong as ColorAmong
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ColorAmong as ColorAmong
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ColorAmong" $ do
  Spec.it s "MkColorAmong, each color" $
    Common.assertCodec
      s
      ColorAmong.codec
      (ColorAmong.MkColorAmong (Filter.HasCardType CardType.Land) Nothing)
      " {\"among\":{\"type\":\"HasCardType\",\"value\":\"Land\"}} "
  Spec.it s "MkColorAmong, one color chosen" $
    Common.assertCodec
      s
      ColorAmong.codec
      (ColorAmong.MkColorAmong (Filter.HasCardType CardType.Land) (Just ColorSpread.OneColor))
      " {\"among\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"spread\":\"OneColor\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ColorAmong.codec
