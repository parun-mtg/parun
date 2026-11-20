{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AddPhases where

import qualified Parun.Codec.ExtraPhase as ExtraPhase
import qualified Parun.Codec.PhaseAnchor as PhaseAnchor
import qualified Parun.Codec.PhaseGrant as PhaseGrant
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AddPhases as AddPhases
import qualified Parun.Types.PhaseAnchor as PhaseAnchor
import qualified Parun.Types.PhaseGrant as PhaseGrant

-- | The anchor defaults to the phase the effect resolves in, and the grant to
-- "there is", what most cards say.
codec :: Codec.Codec AddPhases.AddPhases
codec = Fields.object $ do
  after <- Fields.defaulted "after" PhaseAnchor.ThisPhase PhaseAnchor.codec AddPhases.after
  grant <- Fields.defaulted "grant" PhaseGrant.ThereIs PhaseGrant.codec AddPhases.grant
  phases <- Fields.required "phases" (Common.list ExtraPhase.codec) AddPhases.phases
  pure AddPhases.MkAddPhases {AddPhases.after = after, AddPhases.phases = phases, AddPhases.grant = grant}
