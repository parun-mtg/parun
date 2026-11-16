{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SourceDealtDamage where

import qualified Parun.Codec.DamageKind as DamageKind
import qualified Parun.Codec.RecipientKind as RecipientKind
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

-- | A bare object keyed by the record's field names, as Parun.Codec.MovedBetween
-- is. An absent `kind` is any damage.
codec :: Codec.Codec SourceDealtDamage.SourceDealtDamage
codec = Fields.object $ do
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) SourceDealtDamage.kind
  recipient <- Fields.required "recipient" RecipientKind.codec SourceDealtDamage.recipient
  pure SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = kind, SourceDealtDamage.recipient = recipient}
