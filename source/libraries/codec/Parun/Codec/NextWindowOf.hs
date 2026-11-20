{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.NextWindowOf where

import qualified Parun.Codec.NextWindow as NextWindow
import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.NextWindowOf as NextWindowOf

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec NextWindowOf.NextWindowOf
codec = Fields.object $ do
  window <- Fields.required "window" NextWindow.codec NextWindowOf.window
  player <- Fields.required "player" PlayerRef.codec NextWindowOf.player
  pure NextWindowOf.MkNextWindowOf {NextWindowOf.window = window, NextWindowOf.player = player}
