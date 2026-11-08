module Parun.Types.Violation where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName

-- | One way a deck breaks its format's construction rules.
data Violation
  = -- | CR 100.2a / 100.2b: fewer cards in the deck than the minimum; the
    -- deck's size, then the minimum.
    TooFewCards Natural.Natural Natural.Natural
  | -- | CR 903.5a / 903.12d: more cards in the deck than the maximum; the
    -- deck's size, then the maximum.
    TooManyCards Natural.Natural Natural.Natural
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
  | -- | CR 207.5: a card circling other than as many colours as it instructs,
    -- two or none; the name, the count circled, then the count instructed.
    MiscircledCard CardName.CardName Natural.Natural Natural.Natural
  | -- | CR 315.1: a conspiracy card in a constructed deck or sideboard.
    ConspiracyCard CardName.CardName
  | -- | CR 903.3: a Commander deck that designates no commander.
    NoCommander
  | -- | CR 903.3 / 903.12c / 702.124: a designated card the deck may not have as
    -- a commander, alone or in its pair.
    IllegalCommander CardName.CardName
  | -- | CR 903.5c / 903.5d: a card with a colour, in its colour identity or in
    -- the mana its basic land types make, outside its commander's identity.
    OutsideColorIdentity CardName.CardName
  | -- | CR 123.2a: fewer sticker sheets than the minimum, from a constructed
    -- player who brings any; the number brought, then the minimum.
    TooFewStickerSheets Natural.Natural Natural.Natural
  | -- | CR 123.2a: one sticker sheet a constructed player brings more than
    -- once; its name, then how many.
    DuplicateStickerSheet Text.Text Natural.Natural
  deriving (Eq, Ord, Show)
