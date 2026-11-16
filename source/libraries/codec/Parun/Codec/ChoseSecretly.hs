{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ChoseSecretly where

import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.SecretChoice as SecretChoice
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ChoseSecretly as ChoseSecretly

-- | A bare object keyed by the record's field names. Runtime-only, as
-- Parun.Codec.Voted is.
codec :: Codec.Codec ChoseSecretly.ChoseSecretly
codec = Fields.object $ do
  choice <- Fields.required "choice" SecretChoice.codec ChoseSecretly.choice
  audience <- Fields.required "audience" (Common.set PlayerId.codec) ChoseSecretly.audience
  pure
    ChoseSecretly.MkChoseSecretly
      { ChoseSecretly.choice = choice,
        ChoseSecretly.audience = audience
      }
