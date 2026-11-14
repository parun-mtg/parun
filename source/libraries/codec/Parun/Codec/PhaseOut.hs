{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PhaseOut where

import qualified Parun.Codec.MoveDuration as MoveDuration
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PhaseOut as PhaseOut

-- | The duration is an absent key for CR 702.26a's untap-step schedule.
codec :: Codec.Codec PhaseOut.PhaseOut
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec PhaseOut.ref
  duration <- Fields.defaulted "duration" Nothing (Common.maybe MoveDuration.codec) PhaseOut.duration
  pure PhaseOut.MkPhaseOut {PhaseOut.ref = ref, PhaseOut.duration = duration}
