{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SecretChoice where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SecretChoice as SecretChoice

-- | A bare object keyed by the record's field names. Runtime-only, as
-- Parun.Codec.Voted is.
codec :: Codec.Codec SecretChoice.SecretChoice
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec SecretChoice.player
  object <- Fields.required "object" ObjectId.codec SecretChoice.object
  subtype <- Fields.required "subtype" Subtype.codec SecretChoice.subtype
  pure
    SecretChoice.MkSecretChoice
      { SecretChoice.player = player,
        SecretChoice.object = object,
        SecretChoice.subtype = subtype
      }
