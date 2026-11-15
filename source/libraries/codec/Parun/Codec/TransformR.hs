{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TransformR where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.TransformRewrite as TransformRewrite
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TransformR as TransformR

-- | A bare object keyed by the record's field names, Parun.Codec.AttachR's shape.
codec ::
  (Typeable.Typeable effect) =>
  Codec.Codec effect ->
  Codec.Codec (TransformR.TransformR effect)
codec effectCodec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) TransformR.matching
  rewrite <- Fields.required "rewrite" (TransformRewrite.codec effectCodec) TransformR.rewrite
  pure TransformR.MkTransformR {TransformR.matching = matching, TransformR.rewrite = rewrite}
