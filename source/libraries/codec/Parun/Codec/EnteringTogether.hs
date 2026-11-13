{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.EnteringTogether where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Parun.Codec.CandidateId as CandidateId.Codec
import qualified Parun.Codec.ObjectId as ObjectId.Codec
import qualified Parun.Codec.OptionalDecision as OptionalDecision.Codec
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CandidateId as CandidateId
import qualified Parun.Types.EnteringTogether as EnteringTogether
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision

-- | Every field defaulted to empty, so a batch nothing has entered yet is `{}`.
codec :: Codec.Codec EnteringTogether.EnteringTogether
codec = Fields.object $ do
  arrivals <- Fields.defaulted "arrivals" Seq.empty (Common.seq ObjectId.Codec.codec) EnteringTogether.arrivals
  minted <- Fields.defaulted "minted" Seq.empty (Common.seq ObjectId.Codec.codec) EnteringTogether.minted
  substitutions <- Fields.defaulted "substitutions" Map.empty (Common.keyedList substitution) EnteringTogether.substitutions
  pure
    EnteringTogether.MkEnteringTogether
      { EnteringTogether.arrivals = arrivals,
        EnteringTogether.minted = minted,
        EnteringTogether.substitutions = substitutions
      }

-- | One row's answer, keyed by the row.
substitution :: Codec.Codec (CandidateId.CandidateId, (OptionalDecision.OptionalDecision, Maybe ObjectId.ObjectId))
substitution = Fields.object $ do
  by <- Fields.required "by" CandidateId.Codec.codec fst
  decision <- Fields.required "decision" OptionalDecision.Codec.codec (fst . snd)
  copied <- Fields.defaulted "copied" Nothing (Common.maybe ObjectId.Codec.codec) (snd . snd)
  pure (by, (decision, copied))
