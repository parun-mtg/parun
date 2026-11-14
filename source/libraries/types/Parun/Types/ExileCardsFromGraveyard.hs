module Parun.Types.ExileCardsFromGraveyard where

import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.CostComponent's ExileCardsFromGraveyard arm
-- (#1305): CR 406.2 as a cost, exiling this many matching cards from the paying
-- player's graveyard. Gorex, the Tombshell's "any number" is the announced
-- count (CR 601.2b).
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is.
--
-- count is Parun.Types.Sacrifice's reading and not
-- Parun.Types.TapForTotalPower's: the cards are chosen and counted exactly. The
-- Filter is matched against the card's CR 613 projection, which is the
-- CostComponent arm's note rather than this record's.
data ExileCardsFromGraveyard keyword = MkExileCardsFromGraveyard
  { count :: CostAmount.CostAmount,
    whichCards :: Filter.Filter keyword
  }
  deriving (Eq, Ord, Show)
