module Parun.Types.ChooseQuality where

import qualified Parun.Types.ChosenQuality as ChosenQuality

-- | The payload of Parun.Types.Effect's ChooseQuality arm: CR 608.2d's choice
-- of a quality as a spell or ability resolves, and whose choice it is.
data ChooseQuality = MkChooseQuality
  { quality :: ChosenQuality.ChosenQuality,
    -- | CR 607.2d: whether another ability of the source refers to the choice
    -- (Shapeshifter's "the last chosen number"), which then stands until the
    -- next one. Otherwise it is the resolving ability's own "of your choice"
    -- and ends with the resolution (Distorting Lens).
    linked :: Bool
  }
  deriving (Eq, Ord, Show)
