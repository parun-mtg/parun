module Parun.Types.OfferNamedCopy where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.NamedCopySource as NamedCopySource

-- | CR 707.13: copies of cards defined by name, created outside the game, and
-- the offer of a cast of one of them to the source's controller (CR 707.12).
data OfferNamedCopy = MkOfferNamedCopy
  { -- | Where the copied cards' names come from.
    names :: NamedCopySource.NamedCopySource,
    -- | How many copies: Jhoira of the Ghitu Avatar's "three".
    count :: Natural.Natural,
    -- | What the offer says about the cast (CR 118.9's "without paying its mana
    -- cost").
    offer :: CastOffer.CastOffer
  }
  deriving (Eq, Ord, Show)
