module Parun.Codec.RollTriggerSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Parun.Codec.RollTrigger as RollTrigger
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.RollKind as RollKind
import qualified Parun.Types.RollReading as RollReading
import qualified Parun.Types.RollTrigger as RollTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RollTrigger" $ do
  -- Night Shift of the Living Dead's "whenever you roll a 6": the default kind
  -- is left out.
  Spec.it s "omits any roll" $
    Common.assertCodec
      s
      RollTrigger.codec
      (RollTrigger.MkRollTrigger PlayerRelation.You 6 RollKind.AnyRoll RollReading.FinalResult)
      " {\"roller\":\"You\",\"result\":6} "
  -- "Lifetime" Pass Holder's roll to visit.
  Spec.it s "writes a roll to visit" $
    Common.assertCodec
      s
      RollTrigger.codec
      (RollTrigger.MkRollTrigger PlayerRelation.You 6 RollKind.ToVisit RollReading.FinalResult)
      " {\"roller\":\"You\",\"result\":6,\"roll\":\"ToVisit\"} "
  -- Critical Hit's "when you roll a natural 20".
  Spec.it s "writes a natural result" $
    Common.assertCodec
      s
      RollTrigger.codec
      (RollTrigger.MkRollTrigger PlayerRelation.You 20 RollKind.AnyRoll RollReading.NaturalResult)
      " {\"roller\":\"You\",\"result\":20,\"reading\":\"NaturalResult\"} "
  Spec.it s "rejects a natural roll to visit" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode RollTrigger.codec =<< Common.parse (Text.pack "{\"roller\":\"You\",\"result\":6,\"roll\":\"ToVisit\",\"reading\":\"NaturalResult\"}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s RollTrigger.codec
