{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.TopOfLibrary where

import qualified Parun.Codec.PlayerRef as PlayerRef
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.SideDeck as SideDeck
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.TopOfLibrary as TopOfLibrary

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
--
-- The depth is a full 'Parun.Codec.Quantity' rather than a bare number, so Act on
-- Impulse's three is now @{"type":"Literal","value":3}@ and Commune with Lava's X
-- is an @InSlot@ naming it.
codec :: Codec.Codec TopOfLibrary.TopOfLibrary
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec TopOfLibrary.player
  count <- Fields.required "count" Quantity.codec TopOfLibrary.count
  deck <- Fields.defaulted "deck" Nothing (Common.maybe SideDeck.codec) TopOfLibrary.deck
  pure TopOfLibrary.MkTopOfLibrary {TopOfLibrary.player = player, TopOfLibrary.count = count, TopOfLibrary.deck = deck}
