module Parun.Types.Violation where

import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.SideDeck as SideDeck

-- | One way a deck breaks its format's construction rules.
data Violation
  = -- | CR 100.2a / 100.2b / 718.2 / 901.3 / 904.3: fewer cards in the deck,
    -- or the supplementary deck named, than the minimum; its size, then the
    -- minimum.
    TooFewCards (Maybe SideDeck.SideDeck) Natural.Natural Natural.Natural
  | -- | CR 903.5a / 903.12d: more cards in the deck than the maximum; the
    -- deck's size, then the maximum.
    TooManyCards Natural.Natural Natural.Natural
  | -- | CR 100.2a / 100.4a / 718.2a / 901.3 / 904.3: more cards of one
    -- English name across deck and sideboard, or in the supplementary deck
    -- named, than the limit, a restricted card's one included; the name, the
    -- count, then the limit.
    TooManyCopies (Maybe SideDeck.SideDeck) CardName.CardName Natural.Natural Natural.Natural
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
  | -- | CR 901.3: more phenomenon cards in a planar deck than the maximum;
    -- the count, then the maximum.
    TooManyPhenomena Natural.Natural Natural.Natural
  | -- | CR 718.2a / 901.3 / 904.3 and the Unstable FAQ: a card in a
    -- supplementary deck that is not of the kind the deck holds.
    MisplacedCard SideDeck.SideDeck CardName.CardName
  | -- | CR 902.3: a vanguard designation that is not a vanguard card.
    NotVanguardCard CardName.CardName
  deriving (Eq, Ord, Show)
