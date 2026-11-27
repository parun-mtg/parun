module Parun.Codec.DamageTriggerSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.DamageTrigger as DamageTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.DamageRecipient as DamageRecipient
import qualified Parun.Types.DamageTrigger as DamageTrigger
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.TriggerFrequency as TriggerFrequency

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DamageTrigger" $ do
  -- Spirit Link's host: any kind, any recipient, so no defaulted key.
  Spec.it s "the defaults write no key" $
    Common.assertCodec
      s
      DamageTrigger.codec
      (DamageTrigger.MkDamageTrigger ActingPermanent.Self Nothing Set.empty 0 TriggerFrequency.EveryTime)
      " {\"actor\":{\"type\":\"Self\"}} "
  -- Garruk's Harbinger's "combat damage to a player or planeswalker".
  Spec.it s "a kind and recipients write their keys" $
    Common.assertCodec
      s
      DamageTrigger.codec
      (DamageTrigger.MkDamageTrigger ActingPermanent.Self (Just DamageKind.Combat) (Set.fromList [DamageRecipient.Player (PlayerRef.Relative PlayerRelation.AnyPlayer), DamageRecipient.Permanent CardType.Planeswalker (Filter.And [])]) 0 TriggerFrequency.EveryTime)
      " {\"actor\":{\"type\":\"Self\"},\"kind\":\"Combat\",\"recipients\":[{\"type\":\"Player\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}},{\"type\":\"Permanent\",\"value\":{\"cardType\":\"Planeswalker\"}}]} "
  -- Spinneret and Spiderling's "deals 4 or more damage".
  Spec.it s "a floor writes its key" $
    Common.assertCodec
      s
      DamageTrigger.codec
      (DamageTrigger.MkDamageTrigger ActingPermanent.Self Nothing Set.empty 4 TriggerFrequency.EveryTime)
      " {\"actor\":{\"type\":\"Self\"},\"atLeast\":4} "
  -- Stensia's "for the first time each turn".
  Spec.it s "a frequency writes its key" $
    Common.assertCodec
      s
      DamageTrigger.codec
      (DamageTrigger.MkDamageTrigger ActingPermanent.Self Nothing Set.empty 0 TriggerFrequency.FirstTimeEachTurn)
      " {\"actor\":{\"type\":\"Self\"},\"frequency\":\"FirstTimeEachTurn\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DamageTrigger.codec
