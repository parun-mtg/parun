module Parun.Types.MoveCardsFromGraveyard where

import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.GraveyardCostDestination as GraveyardCostDestination
import qualified Parun.Types.ZoneScope as ZoneScope

-- | The payload of Parun.Types.CostComponent's MoveCardsFromGraveyard arm
-- (#1305): this many matching cards from the graveyards @graveyards@ names
-- (CR 400.1), read against the paying player, exiled (CR 406.2) or put on the
-- bottom of their owner's library (Gurzigost). Gorex, the Tombshell's "any
-- number" is the announced count (CR 601.2b). Every printed "from your
-- graveyard" is CR 109.5's paying player's; Lamplight Phoenix's "exile it"
-- names no graveyard, so it reaches every one and a stolen Phoenix is exiled
-- from its owner's (CR 400.3).
--
-- PARAMETRIC in the keyword for the Filter it carries, exactly as
-- Parun.Types.CostComponent is.
--
-- count is Parun.Types.Sacrifice's reading and not
-- Parun.Types.TapForTotalPower's: the cards are chosen and counted exactly. The
-- Filter is matched against the card's CR 613 projection, which is the
-- CostComponent arm's note rather than this record's.
data MoveCardsFromGraveyard keyword = MkMoveCardsFromGraveyard
  { count :: CostAmount.CostAmount,
    whichCards :: Filter.Filter keyword,
    destination :: GraveyardCostDestination.GraveyardCostDestination,
    graveyards :: ZoneScope.ZoneScope
  }
  deriving (Eq, Ord, Show)
