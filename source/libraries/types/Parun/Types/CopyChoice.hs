module Parun.Types.CopyChoice where

-- | What Parun.Types.OfferNamedCopy offers of the copies it makes.
data CopyChoice
  = -- | CR 707.12: the cast of one of them (Jhoira of the Ghitu Avatar's "You
    -- may cast one of the copies").
    CastOne
  | -- | CR 608.2d: one of them chosen, then a token copy of a permanent card
    -- (CR 111.1) or the cast of an instant or sorcery card (Maelstrom Archangel
    -- Avatar's "choose one of those copies").
    ChooseThenTokenOrCast
  deriving (Bounded, Enum, Eq, Ord, Show)
