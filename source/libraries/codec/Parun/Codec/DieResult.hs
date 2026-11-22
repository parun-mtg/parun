{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DieResult where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.RollReading as RollReading
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DieResult as DieResult
import qualified Parun.Types.RollReading as RollReading.Type

-- | A bare object keyed by the record's field names, the roller written with
-- whichever codec its parameter takes. The reading defaults to the result.
codec :: (Typeable.Typeable player) => Codec.Codec player -> Codec.Codec (DieResult.DieResult player)
codec player = Fields.object $ do
  roller <- Fields.required "roller" player DieResult.roller
  result <- Fields.required "result" Common.natural DieResult.result
  reading <- Fields.defaulted "reading" RollReading.Type.FinalResult RollReading.codec DieResult.reading
  pure DieResult.MkDieResult {DieResult.roller = roller, DieResult.result = result, DieResult.reading = reading}
