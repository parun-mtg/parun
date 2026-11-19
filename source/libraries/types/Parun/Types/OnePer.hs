module Parun.Types.OnePer where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KeywordTally as KeywordTally

-- | CR 608.2d: a quality each pick from among a group answers for one value
-- of, of its own, that no other pick answers for (Parun.Types.ChosenCardFromAmong).
data OnePer
  = -- | Each color (CR 105.1) among the permanents this matches: Sanar,
    -- Innovative First-Year's "for each of those colors".
    ColorAmong (Filter.Filter Keyword.Keyword)
  | -- | Each card type (CR 205.2a), or each among what the tally matches:
    -- Portent of Calamity's "for each card type", Hurkyl, Master Wizard's
    -- "among noncreature spells you've cast this turn".
    CardType (Maybe (KeywordTally.KeywordTally Keyword.Keyword))
  deriving (Eq, Ord, Show)
