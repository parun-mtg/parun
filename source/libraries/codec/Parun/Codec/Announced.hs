{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Announced where

import qualified Parun.Codec.AnnouncedChoice as AnnouncedChoice
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Announced as Announced

-- | A bare object keyed by the record's field names. Runtime-only, as
-- Parun.Codec.ChoseSecretly is.
codec :: Codec.Codec Announced.Announced
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Announced.player
  object <- Fields.required "object" ObjectId.codec Announced.object
  choice <- Fields.required "choice" AnnouncedChoice.codec Announced.choice
  pure
    Announced.MkAnnounced
      { Announced.player = player,
        Announced.object = object,
        Announced.choice = choice
      }
