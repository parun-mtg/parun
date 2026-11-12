module Parun.Types.Equip where

import Numeric.Natural (Natural)
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.EquipTarget as EquipTarget
import qualified Parun.Types.Filter as Filter

-- | The payload of Parun.Types.Keyword's Equip arm: CR 702.6a's "Equip [cost]",
-- plus CR 702.6c's "Equip [quality] creature" and CR 702.6e's "Equip
-- planeswalker" as fields on it.
--
-- PARAMETRIC in the keyword, for Parun.Types.Cycling's reason: the fields name a
-- Cost and a Filter, both of which can name a Keyword, and Keyword names THIS.
-- Only @Equip Keyword.Keyword@ is ever written.
--
-- quality is Nothing for plain equip and Just for CR 702.6c. Fields rather than
-- more Keyword constructors because rules 702.6c and 702.6e change the minted
-- ability's TARGET and nothing else about it (CR 702.6e's "as though that
-- planeswalker were a creature" is the attach it licenses), and rule 702.6e
-- calls the planeswalker form "a variant of the equip ability".
--
-- timesEachTurn is CR 602.5b's rider printed after the keyword, Shredder's
-- Armor's "Activate only once each turn"; Nothing prints none. A count rather
-- than a list of Parun.Types.ActivationRestriction, which imports Keyword
-- (through Parun.Types.Quantity) and so cannot be named here.
data Equip keyword = MkEquip
  { cost :: Cost.Cost keyword,
    quality :: Maybe (Filter.Filter keyword),
    target :: EquipTarget.EquipTarget,
    timesEachTurn :: Maybe Natural
  }
  deriving (Eq, Ord, Show)
