module Parun.Codec.PerTargetSpec where

import qualified Parun.Codec.PerTarget as PerTarget
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PerTarget as PerTarget.Type
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PerTarget" $ do
  -- Battlefield Thaumaturge's once per creature: no `each` on the wire.
  Spec.it s "once per target" $
    Common.assertCodec
      s
      PerTarget.codec
      (PerTarget.Type.MkPerTarget (Filter.HasCardType CardType.Creature) Nothing)
      " {\"matching\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}} "
  -- Warrior's Blades' "for each +1/+1 counter on the creature it targets".
  Spec.it s "a quantity read against each target" $
    Common.assertCodec
      s
      PerTarget.codec
      (PerTarget.Type.MkPerTarget (Filter.HasCardType CardType.Creature) (Just (Quantity.ObjectCounters CounterKind.PlusOnePlusOne)))
      " {\"matching\":{\"type\":\"HasCardType\",\"value\":\"Creature\"},\"each\":{\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PerTarget.codec
