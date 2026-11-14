module Parun.Types.PlayAllowance where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardType as CardType

-- | CR 601.3: how many of the cards one Effect.GrantPlayFromExile covers may be
-- played under it.
data PlayAllowance
  = -- | CR 601.3: this many plays, all told.
    Plays Natural.Natural
  | -- | CR 601.3 / 205.2b: one spell of each card type not in the set.
    EachCardTypeBut (Set.Set CardType.CardType)
  deriving (Eq, Ord, Show)
