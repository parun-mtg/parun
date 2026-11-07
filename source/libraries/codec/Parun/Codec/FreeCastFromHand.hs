{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.FreeCastFromHand where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.FreeCastFromHand as FreeCastFromHand

-- | A bare object keyed by the record's field names. "amount" defaults to
-- Nothing, Parun.Codec.AffectedUnless's posture: almost no grant compares
-- against a computed number.
codec :: Codec.Codec FreeCastFromHand.FreeCastFromHand
codec = Fields.object $ do
  spells <- Fields.required "spells" (Filter.codec Keyword.codec) FreeCastFromHand.spells
  amount <- Fields.defaulted "amount" Nothing (Common.maybe Quantity.codec) FreeCastFromHand.amount
  pure FreeCastFromHand.MkFreeCastFromHand {FreeCastFromHand.spells = spells, FreeCastFromHand.amount = amount}
