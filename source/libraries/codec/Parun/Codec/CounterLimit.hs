{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CounterLimit where

import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CounterLimit as CounterLimit

-- | An object with two required keys: every limit prints both its kind and its
-- N, so neither has a default.
codec :: Codec.Codec CounterLimit.CounterLimit
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) CounterLimit.kind
  atMost <- Fields.required "atMost" Common.natural CounterLimit.atMost
  pure CounterLimit.MkCounterLimit {CounterLimit.kind = kind, CounterLimit.atMost = atMost}
