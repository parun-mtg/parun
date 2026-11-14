module Parun.Types.CostKeyword where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword

-- | A rule-702 keyword whose whole payload is one [cost], named WITHOUT that
-- cost: what Parun.Types.Modification's GainKeywordAtManaCost hands out, priced
-- at the receiving card's own mana cost (CR 202.1a). One constructor per
-- keyword a printed "the [keyword] cost is equal to its mana cost" grants.
data CostKeyword
  = -- | CR 702.34a (Lier, Disciple of the Drowned).
    Flashback
  | -- | CR 702.97a (Varolz, the Scar-Striped).
    Scavenge
  | -- | CR 702.84a (Grixis).
    Unearth
  | -- | CR 702.128a (Cursecloth Wrappings).
    Embalm
  | -- | CR 702.141a (Wire Surgeons).
    Encore
  | -- | CR 702.168a (Disguise Agent).
    Disguise
  | -- | CR 702.152a (Henzie "Toolbox" Torre).
    Blitz
  | -- | CR 702.180a (Songcrafter Mage).
    Harmonize
  | -- | CR 702.187b (Norman Osborn // Green Goblin).
    Mayhem
  | -- | CR 702.56a (Djinn Illuminatus).
    Replicate
  | -- | CR 702.119a (Herigast, Erupting Nullkite).
    Emerge
  | -- | CR 702.196a (Scotty, Miracle Worker), with Parun.Types.BeamMeUp's unreturnable.
    BeamMeUp (Maybe (Filter.Filter Keyword.Keyword))
  deriving (Eq, Ord, Show)
