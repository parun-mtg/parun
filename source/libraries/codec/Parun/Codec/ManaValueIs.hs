{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ManaValueIs where

import qualified Parun.Codec.Reference as Reference
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ManaValueIs as ManaValueIs

codec :: Codec.Codec ManaValueIs.ManaValueIs
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec ManaValueIs.object
  value <- Fields.required "value" Common.integer ManaValueIs.value
  pure ManaValueIs.MkManaValueIs {ManaValueIs.object = object, ManaValueIs.value = value}
