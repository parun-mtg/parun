{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.UpToCasts where

import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.TotalCap as TotalCap
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.UpToCasts as UpToCasts

-- | A bare object keyed by the record's field names, @total@ omitted when
-- Nothing.
codec :: Codec.Codec UpToCasts.UpToCasts
codec = Fields.object $ do
  casts <- Fields.required "casts" Quantity.codec UpToCasts.casts
  total <- Fields.defaulted "total" Nothing (Common.maybe TotalCap.codec) UpToCasts.total
  pure UpToCasts.MkUpToCasts {UpToCasts.casts = casts, UpToCasts.total = total}
