{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Cause where

import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Cause as Cause

-- | An object keyed by the record's field names; "keyword" defaults to
-- Nothing, the ability no keyword represents. Runtime-only.
codec :: Codec.Codec Cause.Cause
codec = Fields.object $ do
  controller <- Fields.required "controller" PlayerId.codec Cause.controller
  source <- Fields.required "source" ObjectId.codec Cause.source
  keyword <- Fields.defaulted "keyword" Nothing (Common.maybe KeywordFamily.codec) Cause.keyword
  pure Cause.MkCause {Cause.controller = controller, Cause.source = source, Cause.keyword = keyword}
