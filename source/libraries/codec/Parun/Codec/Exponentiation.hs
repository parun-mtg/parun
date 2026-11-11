{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Exponentiation where

import qualified Data.Typeable as Typeable
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Exponentiation as Exponentiation

-- | A bare object keyed by the record's field names, Parun.Codec.Times' shape
-- and, for its reason, taking the quantity codec as a parameter.
codec :: (Typeable.Typeable quantity) => Codec.Codec quantity -> Codec.Codec (Exponentiation.Exponentiation quantity)
codec quantityCodec = Fields.object $ do
  base <- Fields.required "base" quantityCodec Exponentiation.base
  power <- Fields.required "exponent" quantityCodec Exponentiation.exponent
  pure Exponentiation.MkExponentiation {Exponentiation.base = base, Exponentiation.exponent = power}
