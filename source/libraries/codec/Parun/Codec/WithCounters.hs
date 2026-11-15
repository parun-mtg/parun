{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.WithCounters where

import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CounterKind as CounterKind.Type
import qualified Parun.Types.Keyword as Keyword.Type
import qualified Parun.Types.Quantity as Quantity.Type
import qualified Parun.Types.WithCounters as WithCounters

-- | An array of kind/count pairs ascending by kind, which is EntryRiders'
-- counter spelling reused rather than re-spelled: the two payloads say the same
-- thing about the same rule (CR 614.1c and CR 122.6), so one wire form serves
-- both. It replaced the single @{"kind":...,"amount":...}@ object when the row
-- grew to several kinds (#2314), which itself replaced a two-element array
-- (#1464).
--
-- 'Common.nonEmptyKeyedList' and not 'Common.keyedList': an empty array would be
-- a row that places nothing, which no card prints.
codec :: Codec.Codec WithCounters.WithCounters
codec =
  Common.wrapper (Common.nonEmptyKeyedList counter) WithCounters.MkWithCounters WithCounters.counters

-- | One counter kind and the count an object enters with, which is a Quantity
-- rather than a number (CR 122.6, CR 107.3): Printlifter Ooze's "X +1/+1
-- counters on it, where X is the number of other creatures you control".
--
-- A PAIR PER KIND, where this used to be 'Common.multiset' -- a plain array with
-- repeats, which can spell a literal count and nothing else.
counter :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Quantity.Type.Quantity)
counter = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  count <- Fields.required "count" Quantity.codec snd
  pure (kind, count)
