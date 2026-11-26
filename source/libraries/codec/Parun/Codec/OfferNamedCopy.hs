{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.OfferNamedCopy where

import qualified Parun.Codec.CastOffer as CastOffer
import qualified Parun.Codec.CopyChoice as CopyChoice
import qualified Parun.Codec.NamedCopySource as NamedCopySource
import qualified Parun.Codec.PayGate as PayGate
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CopyChoice as CopyChoice.Type
import qualified Parun.Types.OfferNamedCopy as OfferNamedCopy

-- | A bare object keyed by the record's field names: one copy, no cost and an
-- ordinary cast of one by default, Garth One-Eye's.
codec :: Codec.Codec OfferNamedCopy.OfferNamedCopy
codec = Fields.object $ do
  names <- Fields.required "names" NamedCopySource.codec OfferNamedCopy.names
  count <- Fields.defaulted "count" 1 Common.natural OfferNamedCopy.count
  offer <- Fields.defaulted "offer" CastOffer.defaultValue CastOffer.codec OfferNamedCopy.offer
  payGate <- Fields.defaulted "payGate" Nothing (Common.maybe PayGate.codec) OfferNamedCopy.payGate
  choice <- Fields.defaulted "choice" CopyChoice.Type.CastOne CopyChoice.codec OfferNamedCopy.choice
  pure
    OfferNamedCopy.MkOfferNamedCopy
      { OfferNamedCopy.names = names,
        OfferNamedCopy.count = count,
        OfferNamedCopy.offer = offer,
        OfferNamedCopy.payGate = payGate,
        OfferNamedCopy.choice = choice
      }
