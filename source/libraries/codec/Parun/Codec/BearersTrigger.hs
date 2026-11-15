{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BearersTrigger where

import qualified Parun.Codec.EntryTriggering as EntryTriggering
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordDesignator as KeywordDesignator
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.BearersTrigger as BearersTrigger

codec :: Codec.Codec BearersTrigger.BearersTrigger
codec = Fields.object $ do
  bearers <- Fields.required "bearers" (Filter.codec Keyword.codec) BearersTrigger.bearers
  abilities <- Fields.defaulted "abilities" Nothing (Common.maybe (KeywordDesignator.codec Keyword.codec)) BearersTrigger.abilities
  triggering <- Fields.required "triggering" EntryTriggering.codec BearersTrigger.triggering
  pure BearersTrigger.MkBearersTrigger {BearersTrigger.bearers = bearers, BearersTrigger.abilities = abilities, BearersTrigger.triggering = triggering}
