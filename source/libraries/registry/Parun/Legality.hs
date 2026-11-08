-- Answering one question: does this deck obey its format's construction rules,
-- and if not, every way it does not?
--
-- Validation before a game, never part of one: Parun.Engine.Setup.newGame
-- takes any deck, and a caller that wants a legal one asks here first. Beside
-- Parun.DeckList because a deck list is the other half of what a player hands
-- over.
module Parun.Legality where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Engine.Card as Card
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Format as Format
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.Violation as Violation

-- | Every violation of the format's rules, from every rule: none stops at the
-- first failure.
check :: Format.Format -> Deck.Deck -> [Violation.Violation]
check format deck = concatMap ($ deck) (rules format)

-- | The construction rules a format imposes, each a predicate over a deck.
rules :: Format.Format -> [Deck.Deck -> [Violation.Violation]]
rules format = case format of
  -- CR 100.2a / 100.4a.
  Format.Constructed -> [atLeast 60, copiesAtMost 4, sideboardAtMost 15]
  -- CR 100.2b: no copy limit beyond the product's own.
  Format.Limited -> [atLeast 40]

-- CR 100.2a / 100.2b: a minimum deck size, counting the deck and not the
-- sideboard (CR 100.4).
atLeast :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
atLeast least deck =
  let size = sum (Deck.cards deck)
   in [Violation.TooFewCards size least | size < least]

-- CR 100.2a / 100.4a: at most this many cards of one English name other than
-- basic land cards, deck and sideboard together. Names are compared as
-- Parun.Engine.Card.combined has them; no card in the pool has an
-- interchangeable name (CR 201.3b), so equal names are the whole of it.
copiesAtMost :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
copiesAtMost limit deck =
  let counted = Map.fromListWith (+) [(Face.name (Card.combined card), count) | (printing, count) <- Map.toList (Map.unionWith (+) (Deck.cards deck) (Deck.sideboard deck)), let card = Printing.card printing, not (basicLand card)]
   in [Violation.TooManyCopies written count limit | (written, count) <- Map.toList counted, count > limit]

-- CR 100.4a: a maximum sideboard size.
sideboardAtMost :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
sideboardAtMost most deck =
  let size = sum (Deck.sideboard deck)
   in [Violation.SideboardTooLarge size most | size > most]

-- CR 205.4c: the supertype makes a basic land, not the name or a basic land
-- type, so snow basics and Wastes are exempt and Dryad Arbor is not.
basicLand :: Card.Type.Card -> Bool
basicLand card =
  let typeLine = Face.typeLine (Card.combined card)
   in Set.member Supertype.Basic (TypeLine.supertypes typeLine) && Set.member CardType.Land (TypeLine.types typeLine)

-- | A violation as a line a person can read.
explain :: Violation.Violation -> Text.Text
explain violation = Text.pack $ case violation of
  Violation.TooFewCards size least -> show size <> " cards in the deck, fewer than " <> show least
  Violation.TooManyCopies written count limit -> show count <> " copies of " <> Text.unpack (CardName.unwrap written) <> ", more than " <> show limit
  Violation.SideboardTooLarge size most -> show size <> " cards in the sideboard, more than " <> show most

-- | A format's name on the command line.
name :: Format.Format -> Text.Text
name format = Text.pack $ case format of
  Format.Constructed -> "constructed"
  Format.Limited -> "limited"

-- | The format a command-line name means.
named :: Text.Text -> Maybe Format.Format
named text = lookup (Text.toLower text) [(name format, format) | format <- [minBound .. maxBound]]
