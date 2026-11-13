module Parun.Types.CardAbilities where

import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LentCards as LentCards

-- | The payload of Parun.Types.Modification's GainCardAbilities arm: where to
-- walk, which cards there lend the object their activated abilities, whether
-- they lend their triggered ones too, and the restrictions each activated one
-- gains.
data CardAbilities = MkCardAbilities
  { from :: LentCards.LentCards,
    -- | Matched against each card `from` walks, framed by the effect's source:
    -- a pile linked to it (Filter.IsExiledWithSource, Myr Welder) or any other
    -- quality of the card (Mairsil, the Pretender's "cards you own in exile
    -- with cage counters on them", Conspicuous Snoop's "a Goblin card").
    cards :: Filter.Filter Keyword.Keyword,
    -- | CR 603.1: whether each lends its triggered abilities as well (Koh, the
    -- Face Stealer's "all activated and triggered abilities").
    triggered :: Bool,
    restrictions :: [ActivationRestriction.ActivationRestriction]
  }
  deriving (Eq, Ord, Show)
