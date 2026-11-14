{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CardsFromHand where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CostAmount as CostAmount
import qualified Parun.Codec.Filter as Filter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CardsFromHand as CardsFromHand

-- | A bare object keyed by the record's field names. The keyword codec is a
-- PARAMETER; see Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CardsFromHand.CardsFromHand keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" CostAmount.codec CardsFromHand.count
  whichCards <- Fields.required "whichCards" (Filter.codec keywordCodec) CardsFromHand.whichCards
  pure CardsFromHand.MkCardsFromHand {CardsFromHand.count = count, CardsFromHand.whichCards = whichCards}
