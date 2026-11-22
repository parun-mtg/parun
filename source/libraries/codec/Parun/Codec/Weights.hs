{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Weights where

import qualified Parun.Codec.Features as Features
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Weights as Weights

codec :: Codec.Codec Weights.Weights
codec = Fields.object $ do
  ours <- Fields.required "ours" Features.codec Weights.ours
  theirs <- Fields.required "theirs" Features.codec Weights.theirs
  pure Weights.MkWeights {Weights.ours = ours, Weights.theirs = theirs}
