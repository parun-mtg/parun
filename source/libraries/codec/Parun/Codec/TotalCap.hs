{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TotalCap where

import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TotalCap as TotalCap

-- | A bare object keyed by the record's field names, both required.
codec :: Codec.Codec TotalCap.TotalCap
codec = Fields.object $ do
  weight <- Fields.required "weight" Quantity.codec TotalCap.weight
  most <- Fields.required "most" Quantity.codec TotalCap.most
  pure TotalCap.MkTotalCap {TotalCap.weight = weight, TotalCap.most = most}
