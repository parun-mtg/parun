{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TurnOrdinal where

import qualified Parun.Codec.TurnScope as TurnScope
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TurnOrdinal as TurnOrdinal

-- | A bare object keyed by the record's field names, both required.
codec :: Codec.Codec TurnOrdinal.TurnOrdinal
codec = Fields.object $ do
  scope <- Fields.required "scope" TurnScope.codec TurnOrdinal.scope
  ordinal <- Fields.required "ordinal" Common.natural TurnOrdinal.ordinal
  pure TurnOrdinal.MkTurnOrdinal {TurnOrdinal.scope = scope, TurnOrdinal.ordinal = ordinal}
