{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RevealedChoice where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RevealedChoice as RevealedChoice

-- | A bare object keyed by the record's field names. Runtime-only, as
-- Parun.Codec.Voted is.
codec :: Codec.Codec RevealedChoice.RevealedChoice
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec RevealedChoice.player
  object <- Fields.required "object" ObjectId.codec RevealedChoice.object
  subtype <- Fields.required "subtype" Subtype.codec RevealedChoice.subtype
  pure
    RevealedChoice.MkRevealedChoice
      { RevealedChoice.player = player,
        RevealedChoice.object = object,
        RevealedChoice.subtype = subtype
      }
