{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LeavesTrigger where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordDesignator as KeywordDesignator
import qualified Parun.Codec.LeavingPermanent as LeavingPermanent
import qualified Parun.Codec.OwnedZone as OwnedZone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LeavesTrigger as LeavesTrigger

-- | A bare object keyed by the record's field names. The source, any
-- destination, no exclusion, EachOccurrence and no activation write no key
-- (LeavesTrigger.defaultValue).
--
-- Unprinted: `whileActivating` with no destination, which would have to see a
-- departure from the game (CR 800.4a), whose GameEvent.LeftTheGame records no
-- activation. It is refused, and Parun.Codec.LeavesTriggerSpec's "rejects
-- whileActivating without a destination" proves the refusal.
codec :: Codec.Codec LeavesTrigger.LeavesTrigger
codec = Fields.objectWith activationHasDestination $ do
  permanent <- Fields.defaulted "permanent" (LeavesTrigger.permanent LeavesTrigger.defaultValue) LeavingPermanent.codec LeavesTrigger.permanent
  destinations <- Fields.defaulted "destinations" (LeavesTrigger.destinations LeavesTrigger.defaultValue) (Common.set OwnedZone.codec) LeavesTrigger.destinations
  excluded <- Fields.defaulted "excluded" (LeavesTrigger.excluded LeavesTrigger.defaultValue) (Common.set OwnedZone.codec) LeavesTrigger.excluded
  batching <- Fields.defaulted "batching" (LeavesTrigger.batching LeavesTrigger.defaultValue) Batching.codec LeavesTrigger.batching
  whileActivating <- Fields.defaulted "whileActivating" Nothing (Common.maybe (KeywordDesignator.codec Keyword.codec)) LeavesTrigger.whileActivating
  pure
    LeavesTrigger.MkLeavesTrigger
      { LeavesTrigger.permanent = permanent,
        LeavesTrigger.destinations = destinations,
        LeavesTrigger.excluded = excluded,
        LeavesTrigger.batching = batching,
        LeavesTrigger.whileActivating = whileActivating
      }

activationHasDestination :: LeavesTrigger.LeavesTrigger -> Either Text.Text LeavesTrigger.LeavesTrigger
activationHasDestination t
  | Maybe.isJust (LeavesTrigger.whileActivating t) && Set.null (LeavesTrigger.destinations t) = Left (Text.pack "LeavesTrigger: whileActivating without a destination is unprinted and not supported")
  | otherwise = Right t
