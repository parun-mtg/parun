{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TurnedFaceUpTrigger where

import qualified Parun.Codec.ActingPermanent as ActingPermanent
import qualified Parun.Codec.PlayerRelation as PlayerRelation
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TurnedFaceUpTrigger as TurnedFaceUpTrigger

-- | A bare object keyed by the record's field names. The bearer and any player
-- write no key (TurnedFaceUpTrigger.defaultValue).
codec :: Codec.Codec TurnedFaceUpTrigger.TurnedFaceUpTrigger
codec = Fields.object $ do
  permanent <- Fields.defaulted "permanent" (TurnedFaceUpTrigger.permanent TurnedFaceUpTrigger.defaultValue) ActingPermanent.codec TurnedFaceUpTrigger.permanent
  player <- Fields.defaulted "player" (TurnedFaceUpTrigger.player TurnedFaceUpTrigger.defaultValue) PlayerRelation.codec TurnedFaceUpTrigger.player
  pure
    TurnedFaceUpTrigger.MkTurnedFaceUpTrigger
      { TurnedFaceUpTrigger.permanent = permanent,
        TurnedFaceUpTrigger.player = player
      }
