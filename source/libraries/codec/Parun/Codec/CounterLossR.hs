{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CounterLossR where

import qualified Parun.Codec.CounterKind as CounterKind
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CounterLossR as CounterLossR

codec :: Codec.Codec CounterLossR.CounterLossR
codec = Fields.object $ do
  onWhat <- Fields.required "onWhat" (Filter.codec Keyword.codec) CounterLossR.onWhat
  whichKind <- Fields.required "whichKind" (CounterKind.codec Keyword.codec) CounterLossR.whichKind
  leaveAtLeast <- Fields.required "leaveAtLeast" Common.natural CounterLossR.leaveAtLeast
  pure
    CounterLossR.MkCounterLossR
      { CounterLossR.onWhat = onWhat,
        CounterLossR.whichKind = whichKind,
        CounterLossR.leaveAtLeast = leaveAtLeast
      }
