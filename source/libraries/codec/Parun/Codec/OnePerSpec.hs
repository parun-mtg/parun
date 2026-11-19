module Parun.Codec.OnePerSpec where

import qualified Parun.Codec.OnePer as OnePer
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CastWindow as CastWindow
import qualified Parun.Types.EventShape as EventShape
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.KeywordTally as KeywordTally
import qualified Parun.Types.OnePer as OnePer
import qualified Parun.Types.Scope as Scope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.OnePer" $ do
  Spec.it s "ColorAmong" $
    Common.assertCodec s OnePer.codec (OnePer.ColorAmong (Filter.HasCardType CardType.Creature)) " {\"type\":\"ColorAmong\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  Spec.it s "CardType, every card type" $
    Common.assertCodec s OnePer.codec (OnePer.CardType Nothing) " {\"type\":\"CardType\"} "
  Spec.it s "CardType, among a tally" $
    Common.assertCodec
      s
      OnePer.codec
      (OnePer.CardType (Just (KeywordTally.MkKeywordTally (Scope.InHistory (EventShape.SpellCast CastWindow.ThisTurn)) (Filter.Not (Filter.HasCardType CardType.Creature)))))
      " {\"type\":\"CardType\",\"value\":{\"scope\":{\"type\":\"InHistory\",\"value\":{\"type\":\"SpellCast\",\"value\":\"ThisTurn\"}},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s OnePer.codec
