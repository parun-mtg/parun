module Parun.Codec.CounterTriggerSpec where

import qualified Parun.Codec.CounterTrigger as CounterTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CounterGrouping as CounterGrouping
import qualified Parun.Types.CounterHolder as CounterHolder
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterMotion as CounterMotion
import qualified Parun.Types.CounterTrigger as CounterTrigger
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.WhichCounters as WhichCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CounterTrigger" $ do
  -- Chandra, Fire Artisan: any loyalty removal from itself, so no defaulted key.
  Spec.it s "the defaults write no key" $
    Common.assertCodec
      s
      CounterTrigger.codec
      (CounterTrigger.MkCounterTrigger (WhichCounters.OfKind CounterKind.Loyalty) CounterMotion.Removed CounterHolder.Self Nothing CounterGrouping.EachPermanent)
      " {\"kind\":{\"type\":\"OfKind\",\"value\":{\"type\":\"Loyalty\"}},\"motion\":{\"type\":\"Removed\"}} "
  -- CR 715.2b's chapter III.
  Spec.it s "a threshold writes its key" $
    Common.assertCodec
      s
      CounterTrigger.codec
      (CounterTrigger.MkCounterTrigger (WhichCounters.OfKind CounterKind.Lore) (CounterMotion.Put PlayerRelation.AnyPlayer) CounterHolder.Self (Just 3) CounterGrouping.EachPermanent)
      " {\"kind\":{\"type\":\"OfKind\",\"value\":{\"type\":\"Lore\"}},\"motion\":{\"type\":\"Put\",\"value\":\"AnyPlayer\"},\"threshold\":3} "
  -- Cloaked Cadet's "one or more +1/+1 counters are put on one or more
  -- creatures".
  Spec.it s "a holder and a grouping write their keys" $
    Common.assertCodec
      s
      CounterTrigger.codec
      (CounterTrigger.MkCounterTrigger (WhichCounters.OfKind CounterKind.PlusOnePlusOne) (CounterMotion.Put PlayerRelation.AnyPlayer) (CounterHolder.Permanent (Filter.HasCardType CardType.Creature)) Nothing CounterGrouping.EachBatch)
      " {\"kind\":{\"type\":\"OfKind\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"motion\":{\"type\":\"Put\",\"value\":\"AnyPlayer\"},\"holder\":{\"type\":\"Permanent\",\"value\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}},\"grouping\":\"EachBatch\"} "
  -- The Great Goblin's "one or more counters", of no kind.
  Spec.it s "any kind writes its tag" $
    Common.assertCodec
      s
      CounterTrigger.codec
      (CounterTrigger.MkCounterTrigger WhichCounters.OfAnyKind (CounterMotion.Put PlayerRelation.You) (CounterHolder.Permanent (Filter.HasSubtype Subtype.Army)) Nothing CounterGrouping.EachPermanent)
      " {\"kind\":{\"type\":\"OfAnyKind\"},\"motion\":{\"type\":\"Put\",\"value\":\"You\"},\"holder\":{\"type\":\"Permanent\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Army\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterTrigger.codec
