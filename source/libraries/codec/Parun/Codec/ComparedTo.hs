{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ComparedTo where

import qualified Parun.Codec.Comparison as Comparison
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ComparedTo as ComparedTo

-- | A bare object keyed by the record's field names, Parun.Codec.Compares's
-- shape less its measured side.
codec :: Codec.Codec ComparedTo.ComparedTo
codec = Fields.object $ do
  comparison <- Fields.required "comparison" Comparison.codec ComparedTo.comparison
  threshold <- Fields.required "threshold" Quantity.codec ComparedTo.threshold
  pure
    ComparedTo.MkComparedTo
      { ComparedTo.comparison = comparison,
        ComparedTo.threshold = threshold
      }
