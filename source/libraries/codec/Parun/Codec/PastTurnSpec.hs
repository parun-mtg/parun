module Parun.Codec.PastTurnSpec where

import qualified Data.Sequence as Seq
import qualified Parun.Codec.PastTurn as PastTurn
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.PastTurn as PastTurn
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.StepBegan as StepBegan

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PastTurn" $ do
  -- CR 805.4: two takers, in an order sorting would undo, so a codec that
  -- kept a set could not pass.
  Spec.it s "a shared turn and its log" $
    Common.assertCodec
      s
      PastTurn.codec
      PastTurn.MkPastTurn
        { PastTurn.activePlayers = [PlayerId.MkPlayerId 2, PlayerId.MkPlayerId 0],
          PastTurn.events =
            Seq.singleton
              LoggedEvent.MkLoggedEvent
                { LoggedEvent.group = EventGroup.next EventGroup.first,
                  LoggedEvent.event = GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) (PlayerId.MkPlayerId 2))
                }
        }
      " {\"activePlayers\":[2,0],\"events\":[{\"group\":1,\"event\":{\"type\":\"StepBegan\",\"value\":{\"phase\":{\"type\":\"Ending\",\"value\":\"EndStep\"},\"player\":2}}}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PastTurn.codec
