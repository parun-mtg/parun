module Parun.Codec.BecomesTargetedTriggerSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.BecomesTargetedTrigger as BecomesTargetedTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.BecomesTargetedTrigger as BecomesTargetedTrigger
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.TargetedSubject as TargetedSubject
import qualified Parun.Types.TriggerFrequency as TriggerFrequency
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.BecomesTargetedTrigger" $ do
  Spec.it s "writes no key for the defaults" $
    Common.assertCodec s BecomesTargetedTrigger.codec BecomesTargetedTrigger.defaultValue " {} "
  -- Ward's: only the relation narrowed.
  Spec.it s "round-trips a Self subject with an opponent's relation" $
    Common.assertCodec
      s
      BecomesTargetedTrigger.codec
      BecomesTargetedTrigger.defaultValue {BecomesTargetedTrigger.relation = PlayerRelation.Opponent}
      " {\"relation\":\"Opponent\"} "
  -- Leyline of Combustion's "you and/or" over Professor Hojo's creatures you
  -- control, Loki, God of Mischief's either ability, one batch, Surrak, Elusive
  -- Hunter's stack beside the battlefield, valiant's first time each turn,
  -- Mirror-Shield Hoplite's backup ability, Brine Comber's Aura spell.
  Spec.it s "round-trips every field" $
    Common.assertCodec
      s
      BecomesTargetedTrigger.codec
      ( BecomesTargetedTrigger.MkBecomesTargetedTrigger
          { BecomesTargetedTrigger.subject = TargetedSubject.PlayerOrPermanent PlayerRelation.You (ActingPermanent.Matching (Filter.And [Filter.HasCardType CardType.Creature, Filter.HeldBy Holder.Controller (ControllerRelation.Related PlayerRelation.You)])),
            BecomesTargetedTrigger.relation = PlayerRelation.Opponent,
            BecomesTargetedTrigger.kinds = Set.fromList [StackObjectKind.ActivatedAbility, StackObjectKind.TriggeredAbility],
            BecomesTargetedTrigger.batching = Batching.OneOrMore,
            BecomesTargetedTrigger.zones = Set.fromList [Zone.Battlefield, Zone.Stack],
            BecomesTargetedTrigger.frequency = TriggerFrequency.FirstTimeEachTurn,
            BecomesTargetedTrigger.sourceKeyword = Just KeywordFamily.Backup,
            BecomesTargetedTrigger.sourceFilter = Just (Filter.HasSubtype Subtype.Aura)
          }
      )
      " {\"subject\":{\"type\":\"PlayerOrPermanent\",\"value\":{\"player\":\"You\",\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":\"Creature\"},{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"You\"}}]}}}},\"relation\":\"Opponent\",\"kinds\":[\"ActivatedAbility\",\"TriggeredAbility\"],\"batching\":\"OneOrMore\",\"zones\":[\"Battlefield\",\"Stack\"],\"frequency\":\"FirstTimeEachTurn\",\"sourceKeyword\":\"Backup\",\"sourceFilter\":{\"type\":\"HasSubtype\",\"value\":\"Aura\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecomesTargetedTrigger.codec
