module Parun.Codec.OnePerSpec where

import qualified Parun.Codec.OnePer as OnePer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.OnePer as OnePer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OnePer" $ do
  Spec.it s "ColorAmong" $
    Common.assertCodec s OnePer.codec (OnePer.ColorAmong (Filter.HasCardType CardType.Creature)) " {\"type\":\"ColorAmong\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "CardType" $
    Common.assertCodec s OnePer.codec OnePer.CardType " {\"type\":\"CardType\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s OnePer.codec
