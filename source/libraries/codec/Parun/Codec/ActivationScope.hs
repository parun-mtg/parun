{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ActivationScope where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActivationScope as ActivationScope

-- | A bare object keyed by the record's field names; `lentBy` defaults to any
-- ability of the source.
codec :: Codec.Codec ActivationScope.ActivationScope
codec = Fields.object $ do
  source <- Fields.required "source" (Filter.codec Keyword.codec) ActivationScope.source
  lentBy <- Fields.defaulted "lentBy" Nothing (Common.maybe (Filter.codec Keyword.codec)) ActivationScope.lentBy
  pure ActivationScope.MkActivationScope {ActivationScope.source = source, ActivationScope.lentBy = lentBy}
