module Parun.Codec.ChosenQualitySpec where

import qualified Parun.Codec.ChosenQuality as ChosenQuality
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChooseBetween as ChooseBetween
import qualified Parun.Types.ChosenQuality as ChosenQuality
import qualified Parun.Types.SubtypeFamily as SubtypeFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ChosenQuality" $ do
  Spec.it s "Colors (Tablet of the Guilds)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      (ChosenQuality.Colors (ChooseBetween.MkChooseBetween 2 2))
      " {\"type\":\"Colors\",\"value\":{\"least\":2,\"most\":2}} "
  Spec.it s "Colors as a range (Scuttlemutt)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      (ChosenQuality.Colors (ChooseBetween.MkChooseBetween 1 5))
      " {\"type\":\"Colors\",\"value\":{\"least\":1,\"most\":5}} "
  Spec.it s "Number (Shapeshifter)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      (ChosenQuality.Number ChooseBetween.MkChooseBetween {ChooseBetween.least = 0, ChooseBetween.most = 7})
      " {\"type\":\"Number\",\"value\":{\"least\":0,\"most\":7}} "
  Spec.it s "Subtype (Deification)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      (ChosenQuality.Subtype SubtypeFamily.PlaneswalkerType)
      " {\"type\":\"Subtype\",\"value\":\"PlaneswalkerType\"} "
  Spec.it s "CardType (Umori, the Collector)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      ChosenQuality.CardType
      " {\"type\":\"CardType\"} "
  Spec.it s "Parity (Gollum, Riddle Master)" $
    Common.assertCodec
      s
      ChosenQuality.codec
      ChosenQuality.Parity
      " {\"type\":\"Parity\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChosenQuality.codec
