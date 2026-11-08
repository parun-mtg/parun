{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PerTarget where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PerTarget as PerTarget

-- | A bare object keyed by the record's field names, @each@ defaulted to
-- Nothing so a once-per-target change writes only its filter.
codec :: Codec.Codec PerTarget.PerTarget
codec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) PerTarget.matching
  each <- Fields.defaulted "each" Nothing (Common.maybe Quantity.codec) PerTarget.each
  pure PerTarget.MkPerTarget {PerTarget.matching = matching, PerTarget.each = each}
