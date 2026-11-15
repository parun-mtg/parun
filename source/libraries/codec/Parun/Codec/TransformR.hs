{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TransformR where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TransformR as TransformR

-- | A bare object keyed by the record's field names, Parun.Codec.AttachR's shape.
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (TransformR.TransformR effect)
codec effectCodec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) TransformR.matching
  effects <- Fields.required "effects" (Common.seq effectCodec) TransformR.effects
  pure TransformR.MkTransformR {TransformR.matching = matching, TransformR.effects = effects}
