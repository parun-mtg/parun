{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Casualty where

import qualified Data.Set as Set
import qualified Parun.Codec.Loyalty as Loyalty
import qualified Parun.Codec.Supertype as Supertype
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Casualty as Casualty

-- | A bare object keyed by the record's field names, Parun.Codec.Equip's shape.
-- @amount@ is null for X; the copy's exceptions are defaulted, so plain casualty
-- N writes neither key.
codec :: Codec.Codec Casualty.Casualty
codec = Fields.object $ do
  amount <- Fields.required "amount" (Common.maybe Common.natural) Casualty.amount
  copyLoses <- Fields.defaulted "copyLoses" Set.empty (Common.set Supertype.codec) Casualty.copyLoses
  copyLoyalty <- Fields.defaulted "copyLoyalty" Nothing (Common.maybe Loyalty.codec) Casualty.copyLoyalty
  pure Casualty.MkCasualty {Casualty.amount = amount, Casualty.copyLoses = copyLoses, Casualty.copyLoyalty = copyLoyalty}
