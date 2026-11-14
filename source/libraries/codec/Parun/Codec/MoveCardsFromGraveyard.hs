{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.MoveCardsFromGraveyard where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CostAmount as CostAmount
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination.Type
import qualified Parun.Types.MoveCardsFromGraveyard as MoveCardsFromGraveyard

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). @destination@ defaults to
-- exile, which every card but Gurzigost and Anurid Scavenger prints. The keyword codec is a PARAMETER; see
-- Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (MoveCardsFromGraveyard.MoveCardsFromGraveyard keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" CostAmount.codec MoveCardsFromGraveyard.count
  whichCards <- Fields.required "whichCards" (Filter.codec keywordCodec) MoveCardsFromGraveyard.whichCards
  destination <- Fields.defaulted "destination" GraveyardCostDestination.Type.Exile GraveyardCostDestination.codec MoveCardsFromGraveyard.destination
  pure MoveCardsFromGraveyard.MkMoveCardsFromGraveyard {MoveCardsFromGraveyard.count = count, MoveCardsFromGraveyard.whichCards = whichCards, MoveCardsFromGraveyard.destination = destination}
