{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.StepOfSeats where

import qualified Parun.Codec.Phase as Phase
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.StepOfSeats as StepOfSeats

-- | A bare object keyed by the record's field names. Runtime-only, as
-- Parun.Codec.Expiry is.
codec :: Codec.Codec StepOfSeats.StepOfSeats
codec = Fields.object $ do
  step <- Fields.required "step" Phase.codec StepOfSeats.step
  seats <- Fields.required "seats" (Common.set PlayerId.codec) StepOfSeats.seats
  pure
    StepOfSeats.MkStepOfSeats
      { StepOfSeats.step = step,
        StepOfSeats.seats = seats
      }
