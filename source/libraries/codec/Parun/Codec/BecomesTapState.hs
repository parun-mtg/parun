{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BecomesTapState where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.Batching as Batching
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.Codec.TapState as TapState
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Batching as Batching.Type
import qualified Parun.Types.BecomesTapState as BecomesTapState
import qualified Parun.Types.TapState as TapState.Type

-- | A bare object keyed by the record's field names. The status and the actor
-- are required; EachOccurrence, any cause, any tapper and no "while" write no
-- key.
--
-- Unprinted: a tapper beside an untap or a cause, which the events carrying
-- those never name. It is refused, and Parun.Codec.BecomesTapStateSpec's
-- "rejects a tapper beside" cases prove the refusal.
codec :: Codec.Codec BecomesTapState.BecomesTapState
codec = Fields.objectWith tapperOnPlainTap $ do
  status <- Fields.required "status" TapState.codec BecomesTapState.status
  actor <- Fields.required "actor" ActingPermanent.codec BecomesTapState.actor
  batching <- Fields.defaulted "batching" Batching.Type.EachOccurrence Batching.codec BecomesTapState.batching
  paying <- Fields.defaulted "paying" Nothing (Common.maybe KeywordFamily.codec) BecomesTapState.paying
  tapper <- Fields.defaulted "tapper" Nothing (Common.maybe PlayerRelation.codec) BecomesTapState.tapper
  while_ <- Fields.defaulted "while" Nothing (Common.maybe Condition.codec) BecomesTapState.while
  pure BecomesTapState.MkBecomesTapState {BecomesTapState.status = status, BecomesTapState.actor = actor, BecomesTapState.batching = batching, BecomesTapState.paying = paying, BecomesTapState.tapper = tapper, BecomesTapState.while = while_}

tapperOnPlainTap :: BecomesTapState.BecomesTapState -> Either Text.Text BecomesTapState.BecomesTapState
tapperOnPlainTap watched
  | Maybe.isJust (BecomesTapState.tapper watched) && (BecomesTapState.status watched == TapState.Type.Untapped || Maybe.isJust (BecomesTapState.paying watched)) = Left (Text.pack "BecomesTapState: a tapper beside an untap or a paying cause is unprinted and not supported")
  | otherwise = Right watched
