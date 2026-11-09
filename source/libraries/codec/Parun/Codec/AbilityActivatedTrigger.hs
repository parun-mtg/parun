{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.AbilityActivatedTrigger where

import qualified Parun.Codec.AbilityKind as AbilityKind
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.AbilityActivatedTrigger as AbilityActivatedTrigger

-- | A bare object keyed by the record's field names; an unnarrowed kind writes
-- no key.
codec :: Codec.Codec AbilityActivatedTrigger.AbilityActivatedTrigger
codec = Fields.object $ do
  filter' <- Fields.required "filter" (Filter.codec Keyword.codec) AbilityActivatedTrigger.filter
  kind <- Fields.defaulted "kind" Nothing (Common.maybe AbilityKind.codec) AbilityActivatedTrigger.kind
  pure
    AbilityActivatedTrigger.MkAbilityActivatedTrigger
      { AbilityActivatedTrigger.filter = filter',
        AbilityActivatedTrigger.kind = kind
      }
