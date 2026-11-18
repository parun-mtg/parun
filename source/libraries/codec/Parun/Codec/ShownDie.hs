{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ShownDie where

import qualified Data.Typeable as Typeable
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ShownDie as ShownDie

-- | A bare object keyed by the record's field names, the result written with
-- whichever codec its parameter takes.
codec :: (Typeable.Typeable result) => Codec.Codec result -> Codec.Codec (ShownDie.ShownDie result)
codec result = Fields.object $ do
  sides <- Fields.required "sides" Common.natural ShownDie.sides
  shown <- Fields.required "result" result ShownDie.result
  pure ShownDie.MkShownDie {ShownDie.sides = sides, ShownDie.result = shown}
