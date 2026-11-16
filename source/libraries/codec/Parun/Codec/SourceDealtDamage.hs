{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SourceDealtDamage where

import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Parun.Codec.DamageKind as DamageKind
import qualified Parun.Codec.DamageRecipient as DamageRecipient
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SourceDealtDamage as SourceDealtDamage

-- | A bare object keyed by the record's field names, as Parun.Codec.MovedBetween
-- is. An absent `kind` is any damage, and absent `recipients` any recipient.
codec :: (Typeable.Typeable keyword, Ord keyword) => Codec.Codec keyword -> Codec.Codec (SourceDealtDamage.SourceDealtDamage keyword)
codec keywordCodec = Fields.object $ do
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) SourceDealtDamage.kind
  recipients <- Fields.defaulted "recipients" Set.empty (Common.set (DamageRecipient.codec keywordCodec)) SourceDealtDamage.recipients
  pure SourceDealtDamage.MkSourceDealtDamage {SourceDealtDamage.kind = kind, SourceDealtDamage.recipients = recipients}
