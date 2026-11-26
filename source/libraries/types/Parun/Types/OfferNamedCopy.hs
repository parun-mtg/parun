module Parun.Types.OfferNamedCopy where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CastOffer as CastOffer
import qualified Parun.Types.CopyChoice as CopyChoice
import qualified Parun.Types.NamedCopySource as NamedCopySource
import qualified Parun.Types.PayGate as PayGate

-- | CR 707.13: copies of cards defined by name, created outside the game, and
-- the offer of a cast of one of them to the source's controller (CR 707.12).
data OfferNamedCopy = MkOfferNamedCopy
  { -- | Where the copied cards' names come from.
    names :: NamedCopySource.NamedCopySource,
    -- | How many copies, per object where the names come from objects: Jhoira of
    -- the Ghitu Avatar's "three".
    count :: Natural.Natural,
    -- | What the offer says about the cast (CR 118.9's "without paying its mana
    -- cost").
    offer :: CastOffer.CastOffer,
    -- | CR 118.12: a cost the controller may pay once the copies are made, the
    -- offer standing only if they do (Maelstrom Archangel Avatar's "You may pay
    -- {3}. If you do").
    payGate :: Maybe PayGate.PayGate,
    -- | What is offered of the copies.
    choice :: CopyChoice.CopyChoice
  }
  deriving (Eq, Ord, Show)
