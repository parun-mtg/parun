module Parun.Types.TokenCopy where

import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 707.1 / 707.8a: what a copy token is minted with, read off the object
-- it copies.
data TokenCopy = MkTokenCopy
  { -- | CR 707.2: the copied object's copiable values, which are what the
    -- token has from the instant it exists.
    values :: ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 707.8a: the other face's, the copied double-faced object's copiable
    -- values with it turned over. Nothing for a copy of something that can't
    -- turn.
    turned :: Maybe ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 707.8a: the face the token enters with up, the one the copied
    -- double-faced object shows. Nothing for its front face.
    face :: Maybe CardName.CardName
  }
  deriving (Eq, Ord, Show)
