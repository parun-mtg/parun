module Parun.Types.Violation where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName

-- | One way a deck breaks its format's construction rules.
data Violation
  = -- | CR 100.2a / 100.2b: fewer cards in the deck than the minimum; the
    -- deck's size, then the minimum.
    TooFewCards Natural.Natural Natural.Natural
  | -- | CR 100.2a / 100.4a: more cards of one English name across deck and
    -- sideboard than the limit, a restricted card's one included; the name, the
    -- count, then the limit.
    TooManyCopies CardName.CardName Natural.Natural Natural.Natural
  | -- | CR 100.4a: more cards in the sideboard than the maximum; its size, then
    -- the maximum.
    SideboardTooLarge Natural.Natural Natural.Natural
  | -- | A card the format's pool bans.
    BannedCard CardName.CardName
  | -- | A card the format's pool does not have.
    NotLegalCard CardName.CardName
  | -- | A card the legality table does not have, so its standing is unknown.
    NoLegalityData CardName.CardName
  | -- | CR 407.3: an ante card in a deck or sideboard of a game not for ante.
    AnteCard CardName.CardName
  | -- | CR 315.1: a conspiracy card in a constructed deck or sideboard.
    ConspiracyCard CardName.CardName
  deriving (Eq, Ord, Show)
