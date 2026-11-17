module Parun.Codec.LeavesTriggerSpec where

import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.LeavesTrigger as LeavesTrigger
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.KeywordDesignator as KeywordDesignator
import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.LeavesTrigger as LeavesTrigger
import qualified Parun.Types.LeavingPermanent as LeavingPermanent
import qualified Parun.Types.OwnedZone as OwnedZone
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LeavesTrigger" $ do
  Spec.it s "writes no key for the defaults" $
    Common.assertCodec s LeavesTrigger.codec LeavesTrigger.defaultValue " {} "
  Spec.it s "round-trips a destination" $
    Common.assertCodec
      s
      LeavesTrigger.codec
      (LeavesTrigger.dies LeavingPermanent.Self)
      " {\"destinations\":[{\"zone\":\"Graveyard\"}]} "
  Spec.it s "round-trips an owned destination" $
    Common.assertCodec
      s
      LeavesTrigger.codec
      LeavesTrigger.defaultValue {LeavesTrigger.destinations = Set.singleton (OwnedZone.MkOwnedZone Zone.Graveyard PlayerRelation.You)}
      " {\"destinations\":[{\"owner\":\"You\",\"zone\":\"Graveyard\"}]} "
  Spec.it s "round-trips an exclusion" $
    Common.assertCodec
      s
      LeavesTrigger.codec
      (LeavesTrigger.withoutDying LeavingPermanent.Self)
      " {\"excluded\":[{\"zone\":\"Graveyard\"}]} "
  Spec.it s "round-trips every field" $
    Common.assertCodec
      s
      LeavesTrigger.codec
      (LeavesTrigger.MkLeavesTrigger (LeavingPermanent.Matching (Filter.Not (Filter.HasCardType CardType.Creature))) (Set.singleton (OwnedZone.MkOwnedZone Zone.Hand PlayerRelation.AnyPlayer)) (Set.singleton (OwnedZone.MkOwnedZone Zone.Graveyard PlayerRelation.AnyPlayer)) Batching.OneOrMore (Just (KeywordDesignator.OfFamily KeywordFamily.Craft)))
      " {\"batching\":\"OneOrMore\",\"destinations\":[{\"zone\":\"Hand\"}],\"excluded\":[{\"zone\":\"Graveyard\"}],\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}}},\"whileActivating\":{\"type\":\"OfFamily\",\"value\":\"Craft\"}} "
  -- Unprinted, and refused: a departure from the game records no activation.
  Spec.it s "rejects whileActivating without a destination" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode LeavesTrigger.codec =<< Common.parse (Text.pack "{\"whileActivating\":{\"type\":\"OfFamily\",\"value\":\"Craft\"}}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s LeavesTrigger.codec
