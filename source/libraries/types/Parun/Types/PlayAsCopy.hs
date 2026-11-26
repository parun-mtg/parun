module Parun.Types.PlayAsCopy where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | CR 305.1 / 707.2: the payload of Parun.Types.PlayerEffect's PlayAsCopy arm
-- -- Dakkon Blackblade Avatar's "you may play any colored card from your hand
-- as a copy of a basic land card chosen at random that can produce mana of one
-- of the card's colors". The card played is bound at
-- Parun.Engine.Binding.playedLand while `original`'s filter is read, so
-- Filter.ProducesColorOf names it.
data PlayAsCopy = MkPlayAsCopy
  { -- | Which cards in the player's hand may be played this way.
    cards :: Filter.Filter Keyword.Keyword,
    -- | CR 108.1: which of the Oracle reference's cards one is chosen among at
    -- random, matched against the printed card (CR 400.11).
    original :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
