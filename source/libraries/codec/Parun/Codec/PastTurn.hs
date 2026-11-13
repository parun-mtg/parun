{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PastTurn where

import qualified Parun.Codec.LoggedEvent as LoggedEvent
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PastTurn as PastTurn

-- | A bare object keyed by the record's field names. Runtime-only: a past turn
-- serialises game states, never card data.
codec :: Codec.Codec PastTurn.PastTurn
codec = Fields.object $ do
  activePlayers <- Fields.required "activePlayers" (Common.list PlayerId.codec) PastTurn.activePlayers
  events <- Fields.required "events" (Common.seq LoggedEvent.codec) PastTurn.events
  pure
    PastTurn.MkPastTurn
      { PastTurn.activePlayers = activePlayers,
        PastTurn.events = events
      }
