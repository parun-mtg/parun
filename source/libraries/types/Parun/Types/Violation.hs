module Parun.Types.Violation where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName

-- | One way a deck breaks its format's construction rules.
data Violation
  = -- | CR 100.2a / 100.2b: fewer cards in the deck than the minimum; the
    -- deck's size, then the minimum.
    TooFewCards Natural.Natural Natural.Natural
  | -- | CR 100.2a / 100.4a: more cards of one English name across deck and
    -- sideboard than the limit; the name, the count, then the limit.
    TooManyCopies CardName.CardName Natural.Natural Natural.Natural
  | -- | CR 100.4a: more cards in the sideboard than the maximum; its size, then
    -- the maximum.
    SideboardTooLarge Natural.Natural Natural.Natural
  deriving (Eq, Ord, Show)
