{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.OtherCandidates where

import qualified Parun.Codec.CandidateScope as CandidateScope
import qualified Parun.Codec.Measure as Measure
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.OtherCandidates as OtherCandidates

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec OtherCandidates.OtherCandidates
codec = Fields.object $ do
  measure <- Fields.required "measure" Measure.codec OtherCandidates.measure
  scope <- Fields.required "scope" CandidateScope.codec OtherCandidates.scope
  pure OtherCandidates.MkOtherCandidates {OtherCandidates.measure = measure, OtherCandidates.scope = scope}
