{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Limits where

import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Limits as Limits

codec :: Codec.Codec Limits.Limits
codec = Fields.object $ do
  objects <- Fields.required "objects" Common.natural Limits.objects
  pure Limits.MkLimits {Limits.objects = objects}
