{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RemoveCountersAmong where

import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.RemovalCount as RemovalCount
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.Codec.WhichCounters as WhichCounters
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RemoveCountersAmong as RemoveCountersAmong

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.Effect's RemoveCountersAmong arm. The tally slot is
-- elided when absent, Parun.Codec.RemoveCounters' posture.
--
-- Every count is accepted for counters of one kind or any: an exact one (Spinal
-- Parasite), "any number" (Eventide's Shadow), "up to" a count (Mabel, Bitter
-- Recluse) and "all" (Suncleanser; Oblivion Stone's one kind over every
-- permanent).
codec :: Codec.Codec RemoveCountersAmong.RemoveCountersAmong
codec = Fields.object $ do
  count <- Fields.required "count" RemovalCount.codec RemoveCountersAmong.count
  from <- Fields.required "from" ObjectRef.codec RemoveCountersAmong.from
  kind <- Fields.required "kind" (WhichCounters.codec Keyword.codec) RemoveCountersAmong.kind
  tally <- Fields.defaulted "tally" Nothing (Common.maybe SlotName.codec) RemoveCountersAmong.tally
  pure
    RemoveCountersAmong.MkRemoveCountersAmong
      { RemoveCountersAmong.count = count,
        RemoveCountersAmong.from = from,
        RemoveCountersAmong.kind = kind,
        RemoveCountersAmong.tally = tally
      }
