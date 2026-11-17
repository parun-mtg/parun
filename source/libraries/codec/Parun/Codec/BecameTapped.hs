{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BecameTapped where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.BecameTapped as BecameTapped

-- | A bare object keyed by the record's field names.
-- Runtime-only: GameEvent serialises transcripts, never card data.
codec :: Codec.Codec BecameTapped.BecameTapped
codec = Fields.object $ do
  permanent <- Fields.required "permanent" ObjectId.codec BecameTapped.permanent
  tapper <- Fields.required "tapper" PlayerId.codec BecameTapped.tapper
  pure
    BecameTapped.MkBecameTapped
      { BecameTapped.permanent = permanent,
        BecameTapped.tapper = tapper
      }
