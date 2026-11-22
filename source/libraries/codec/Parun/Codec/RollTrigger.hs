{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RollTrigger where

import qualified Data.Text as Text
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.RollKind as RollKind
import qualified Parun.Codec.RollReading as RollReading
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RollKind as RollKind.Type
import qualified Parun.Types.RollReading as RollReading.Type
import qualified Parun.Types.RollTrigger as RollTrigger

-- | A bare object keyed by the record's field names. The kind defaults to any
-- roll, the reading of a bare "whenever you roll", and the reading to the
-- result, since only a natural result says so ("a natural 20").
--
-- Unprinted: a natural result read off a roll to visit. It is refused, and
-- Parun.Codec.RollTriggerSpec's "rejects a natural roll to visit" proves the
-- refusal.
codec :: Codec.Codec RollTrigger.RollTrigger
codec = Fields.objectWith visitReadsTheResult $ do
  roller <- Fields.required "roller" PlayerRelation.codec RollTrigger.roller
  result <- Fields.required "result" Common.natural RollTrigger.result
  roll <- Fields.defaulted "roll" RollKind.Type.AnyRoll RollKind.codec RollTrigger.roll
  reading <- Fields.defaulted "reading" RollReading.Type.FinalResult RollReading.codec RollTrigger.reading
  pure RollTrigger.MkRollTrigger {RollTrigger.roller = roller, RollTrigger.result = result, RollTrigger.roll = roll, RollTrigger.reading = reading}

visitReadsTheResult :: RollTrigger.RollTrigger -> Either Text.Text RollTrigger.RollTrigger
visitReadsTheResult watched
  | RollTrigger.roll watched == RollKind.Type.ToVisit && RollTrigger.reading watched == RollReading.Type.NaturalResult = Left (Text.pack "RollTrigger: a natural result of a roll to visit is unprinted and not supported")
  | otherwise = Right watched
