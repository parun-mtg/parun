{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.FreeCast where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.FreeCast as FreeCast

-- | A bare object keyed by the record's field names. "amount" defaults to
-- Nothing, Parun.Codec.AffectedUnless's posture: almost no grant compares
-- against a computed number. "from" defaults to Nothing, no zone named.
codec :: Codec.Codec FreeCast.FreeCast
codec = Fields.object $ do
  spells <- Fields.required "spells" (Filter.codec Keyword.codec) FreeCast.spells
  amount <- Fields.defaulted "amount" Nothing (Common.maybe Quantity.codec) FreeCast.amount
  from <- Fields.defaulted "from" Nothing (Common.maybe Zone.codec) FreeCast.from
  pure FreeCast.MkFreeCast {FreeCast.spells = spells, FreeCast.amount = amount, FreeCast.from = from}
