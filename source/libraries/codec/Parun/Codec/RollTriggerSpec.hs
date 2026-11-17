module Parun.Codec.RollTriggerSpec where

import qualified Parun.Codec.RollTrigger as RollTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.RollKind as RollKind
import qualified Parun.Types.RollTrigger as RollTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollTrigger" $ do
  -- Night Shift of the Living Dead's "whenever you roll a 6": the default kind
  -- is left out.
  Spec.it s "omits any roll" $
    Common.assertCodec
      s
      RollTrigger.codec
      (RollTrigger.MkRollTrigger PlayerRelation.You 6 RollKind.AnyRoll)
      " {\"roller\":\"You\",\"result\":6} "
  -- "Lifetime" Pass Holder's roll to visit.
  Spec.it s "writes a roll to visit" $
    Common.assertCodec
      s
      RollTrigger.codec
      (RollTrigger.MkRollTrigger PlayerRelation.You 6 RollKind.ToVisit)
      " {\"roller\":\"You\",\"result\":6,\"roll\":\"ToVisit\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RollTrigger.codec
