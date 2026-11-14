{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.FreeCast where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.TurnOrdinal as TurnOrdinal
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.FreeCast as FreeCast

-- | A bare object keyed by the record's field names. "amount" defaults to
-- Nothing, Parun.Codec.AffectedUnless's posture: almost no grant compares
-- against a computed number. "from" defaults to Nothing, no zone named;
-- "onlyNth" to Nothing, no "first".
--
-- Refused ('Fields.objectWith''s check): `onlyNth` beside `amount`, since the
-- earlier casts it counts are matched with no amount framed -- Scryfall
-- o:"first" o:"without paying" o:"each turn", 2026-11-14, finds no such grant.
codec :: Codec.Codec FreeCast.FreeCast
codec = Fields.objectWith check $ do
  spells <- Fields.required "spells" (Filter.codec Keyword.codec) FreeCast.spells
  amount <- Fields.defaulted "amount" Nothing (Common.maybe Quantity.codec) FreeCast.amount
  from <- Fields.defaulted "from" Nothing (Common.maybe Zone.codec) FreeCast.from
  onlyNth <- Fields.defaulted "onlyNth" Nothing (Common.maybe TurnOrdinal.codec) FreeCast.onlyNth
  pure FreeCast.MkFreeCast {FreeCast.spells = spells, FreeCast.amount = amount, FreeCast.from = from, FreeCast.onlyNth = onlyNth}
  where
    check grant
      | Maybe.isJust (FreeCast.amount grant) && Maybe.isJust (FreeCast.onlyNth grant) = Left (Text.pack "FreeCast: onlyNth beside amount has no printing")
      | otherwise = Right grant
