{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.Enlisted where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.Enlisted as Enlisted

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Enlisted.Enlisted
codec = Fields.object $ do
  enlister <- Fields.required "enlister" ObjectId.codec Enlisted.enlister
  enlisted <- Fields.required "enlisted" ObjectId.codec Enlisted.enlisted
  pure
    Enlisted.MkEnlisted
      { Enlisted.enlister = enlister,
        Enlisted.enlisted = enlisted
      }
