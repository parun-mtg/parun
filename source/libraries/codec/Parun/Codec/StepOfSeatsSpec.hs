module Parun.Codec.StepOfSeatsSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.StepOfSeats as StepOfSeats
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.StepOfSeats as StepOfSeats

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.StepOfSeats" $ do
  -- CR 611.2a.
  Spec.it s "MkStepOfSeats, both keys, seats ascending" $
    Common.assertCodec
      s
      StepOfSeats.codec
      ( StepOfSeats.MkStepOfSeats
          { StepOfSeats.step = Phase.Ending EndingStep.EndStep,
            StepOfSeats.seats = Set.fromList [PlayerId.MkPlayerId 2, PlayerId.MkPlayerId 0]
          }
      )
      " {\"step\":{\"type\":\"Ending\",\"value\":\"EndStep\"},\"seats\":[0,2]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s StepOfSeats.codec
