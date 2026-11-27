{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DamageTrigger where

import qualified Data.Set as Set
import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.DamageKind as DamageKind
import qualified Parun.Codec.DamageRecipient as DamageRecipient
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.TriggerFrequency as TriggerFrequency
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DamageTrigger as DamageTrigger
import qualified Parun.Types.TriggerFrequency as TriggerFrequencyType

-- | A bare object keyed by the record's field names. Any kind, any
-- recipient, any amount and every occurrence write no key.
codec :: Codec.Codec DamageTrigger.DamageTrigger
codec = Fields.object $ do
  actor <- Fields.required "actor" ActingPermanent.codec DamageTrigger.actor
  kind <- Fields.defaulted "kind" Nothing (Common.maybe DamageKind.codec) DamageTrigger.kind
  recipients <- Fields.defaulted "recipients" Set.empty (Common.set (DamageRecipient.codec Keyword.codec)) DamageTrigger.recipients
  atLeast <- Fields.defaulted "atLeast" 0 Common.natural DamageTrigger.atLeast
  frequency <- Fields.defaulted "frequency" TriggerFrequencyType.EveryTime TriggerFrequency.codec DamageTrigger.frequency
  pure
    DamageTrigger.MkDamageTrigger
      { DamageTrigger.actor = actor,
        DamageTrigger.kind = kind,
        DamageTrigger.recipients = recipients,
        DamageTrigger.atLeast = atLeast,
        DamageTrigger.frequency = frequency
      }
