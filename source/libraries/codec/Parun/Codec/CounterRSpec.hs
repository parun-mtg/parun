module Parun.Codec.CounterRSpec where

import qualified Parun.Codec.CounterR as CounterR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterPattern as CounterPattern
import qualified Parun.Types.CounterR as CounterR
import qualified Parun.Types.CounterSubject as CounterSubject
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CounterR" $ do
  -- CR 614.16: Hardened Scales adds one more +1/+1 counter.
  Spec.it s "MkCounterR" $
    Common.assertCodec
      s
      CounterR.codec
      ( CounterR.MkCounterR
          { CounterR.matching =
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Just CounterKind.PlusOnePlusOne,
                  CounterPattern.subject = CounterSubject.ByAnything,
                  CounterPattern.whose = ControllerRelation.Related PlayerRelation.You,
                  CounterPattern.onWhat = Filter.HasCardType CardType.Creature,
                  CounterPattern.onWho = Nothing,
                  CounterPattern.whichPlayerKind = Nothing
                },
            CounterR.scaling = Scaling.AddMore (Quantity.Literal 1),
            CounterR.barsAdditional = False
          }
      )
      " {\"matching\":{\"whichKind\":{\"type\":\"PlusOnePlusOne\"},\"subject\":{\"type\":\"ByAnything\"},\"whose\":{\"type\":\"You\"},\"onWhat\":{\"type\":\"HasCardType\",\"value\":\"Creature\"}},\"scaling\":{\"type\":\"AddMore\",\"value\":{\"type\":\"Literal\",\"value\":1}}} "
  -- CR 614.1a: Melira, the Living Cure -- one poison counter instead, and no
  -- more this turn.
  Spec.it s "Melira, the Living Cure (a player kind, exactly one, barred after)" $
    Common.assertCodec
      s
      CounterR.codec
      ( CounterR.MkCounterR
          { CounterR.matching =
              CounterPattern.MkCounterPattern
                { CounterPattern.whichKind = Nothing,
                  CounterPattern.subject = CounterSubject.ByAnything,
                  CounterPattern.whose = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  CounterPattern.onWhat = Filter.Or [],
                  CounterPattern.onWho = Just (ControllerRelation.Related PlayerRelation.You),
                  CounterPattern.whichPlayerKind = Just PlayerCounterKind.Poison
                },
            CounterR.scaling = Scaling.Exactly (Quantity.Literal 1),
            CounterR.barsAdditional = True
          }
      )
      " {\"matching\":{\"subject\":{\"type\":\"ByAnything\"},\"onWhat\":{\"type\":\"Or\",\"value\":[]},\"onWho\":{\"type\":\"You\"},\"whichPlayerKind\":\"Poison\"},\"scaling\":{\"type\":\"Exactly\",\"value\":{\"type\":\"Literal\",\"value\":1}},\"barsAdditional\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounterR.codec
