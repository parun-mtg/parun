{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.MoveThisFrom where

import qualified Parun.Codec.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination.Type
import qualified Parun.Types.MoveThisFrom as MoveThisFrom

-- | A bare object keyed by the record's field names. @destination@ defaults to
-- exile, which every card but Daniels, Temporal Agent prints,
-- Parun.Codec.MoveCardsFromGraveyard's default.
codec :: Codec.Codec MoveThisFrom.MoveThisFrom
codec = Fields.object $ do
  zone <- Fields.required "zone" Zone.codec MoveThisFrom.zone
  destination <- Fields.defaulted "destination" GraveyardCostDestination.Type.Exile GraveyardCostDestination.codec MoveThisFrom.destination
  pure MoveThisFrom.MkMoveThisFrom {MoveThisFrom.zone = zone, MoveThisFrom.destination = destination}
