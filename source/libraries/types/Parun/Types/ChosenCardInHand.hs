module Parun.Types.ChosenCardInHand where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.HowMany as HowMany
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.OnePer as OnePer
import qualified Parun.Types.PlayerRef as PlayerRef

-- | CR 402.3 with a choice: whose hand is looked in -- which is also who picks --
-- what may be picked, and HOW MANY.

-- ONE PlayerRef where Parun.Types.ChosenCardInGraveyard needs a Chooser beside a
-- PlayerScope, because CR 402.3 collapses the chooser and the zone's owner onto
-- one seat. The fields are named rather than positional for that type's reason.
--
-- The COUNT is Parun.Types.ChosenCardFromAmong's HowMany, per hand, of DISTINCT
-- cards (CR 608.2c's rules of English), so Aquatic Subtlety's "two cards from
-- your hand" is one instruction over two cards (CR 608.2f) rather than two
-- choices that may name
-- the same card, and The Great Aurora's "any number of land cards" is UpTo
-- Nothing. CR 609.3 covers the shortfall.
--
-- The ONEPER is Parun.Types.ChosenCardFromAmong's: Only I Know What Awaits'
-- "for each card type among permanents your opponents control".
data ChosenCardInHand = MkChosenCardInHand
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    count :: HowMany.HowMany,
    onePer :: Maybe OnePer.OnePer
  }
  deriving (Eq, Ord, Show)
