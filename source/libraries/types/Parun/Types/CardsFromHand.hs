module Parun.Types.CardsFromHand where

import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.CostComponent's ExileCardsFromHand and
-- RevealCardsFromHand arms: exactly this many cards the payer chooses from their
-- own hand among the ones the Filter admits. Cadaverous Bloom's "Exile a card
-- from your hand" is a printed count, and March of Wretched Sorrow's "exile any
-- number of black cards" and Martyr of Sands' "Reveal X white cards" announced
-- ones (CR 601.2b).
data CardsFromHand keyword = MkCardsFromHand
  { count :: CostAmount.CostAmount,
    whichCards :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
