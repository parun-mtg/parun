-- Answering one question: does this deck obey its format's construction rules,
-- and if not, every way it does not?
--
-- Validation before a game, never part of one: Parun.Engine.Setup.newGame
-- takes any deck, and a caller that wants a legal one asks here first. Beside
-- Parun.DeckList because a deck list is the other half of what a player hands
-- over.
module Parun.Legality where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Numeric.Natural as Natural
import qualified Parun.Codec.Format as Format.Codec
import qualified Parun.Codec.Legality as Legality.Codec
import qualified Parun.Codec.LegalityTable as LegalityTable.Codec
import qualified Parun.Engine.Ante as Ante
import qualified Parun.Engine.Card as Card
import qualified Parun.Ingest as Ingest
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.DeckLimit as DeckLimit
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Format as Format
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.Legality as Legality
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Types.Violation as Violation
import qualified Paths_parun as Paths
import qualified System.Directory as Directory

-- | Every violation of the format's rules and pool, for a game played with the
-- settings given, from every rule: none stops at the first failure.
check :: LegalityTable.LegalityTable -> GameSettings.GameSettings -> Format.Format -> Deck.Deck -> [Violation.Violation]
check table settings format deck = concatMap ($ deck) (rules table settings format)

-- | The construction rules a format imposes, each a predicate over a deck.
rules :: LegalityTable.LegalityTable -> GameSettings.GameSettings -> Format.Format -> [Deck.Deck -> [Violation.Violation]]
rules table settings format =
  let standing = fmap (poolStanding table) (Format.pool format)
      restricted card = any (\standingOf -> standingOf card == Just (Just Legality.Restricted)) standing
   in case Format.rules format of
        -- CR 100.2a / 100.4a.
        RuleFamily.Constructed -> [atLeast 60, copiesAtMost restricted (Just 4) bothSections, sideboardAtMost 15] <> foldMap (pure . inPool) standing <> [anteCards settings, conspiracies]
        -- CR 100.2b: no copy limit beyond the product's own, but a card's own
        -- "up to N" still caps the deck: Nazgûl's ruling of 2023-06-16 keeps a
        -- tenth in the sideboard.
        RuleFamily.Limited -> [atLeast 40, copiesAtMost restricted Nothing Deck.cards] <> foldMap (pure . inPool) standing <> [anteCards settings]

-- Deck and sideboard together: CR 100.4a counts the sideboard, and a card
-- barred from the deck is barred from both (CR 407.3).
bothSections :: Deck.Deck -> Map.Map Printing.Printing Natural.Natural
bothSections = Map.unionWith (+) <$> Deck.cards <*> Deck.sideboard

-- CR 100.2a / 100.2b: a minimum deck size, counting the deck and not the
-- sideboard (CR 100.4).
atLeast :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
atLeast least deck =
  let size = sum (Deck.cards deck)
   in [Violation.TooFewCards size least | size < least]

-- CR 100.2a / 100.4a: at most so many cards of one English name, among the
-- cards `counted` picks: one for a card the pool restricts, else the card's
-- own deckLimit where it prints one, else the format's limit, and none for a
-- basic land card. Names are compared as Parun.Engine.Card.combined has them;
-- no card in the pool has an interchangeable name (CR 201.3b), so equal names
-- are the whole of it.
copiesAtMost :: (Card.Type.Card -> Bool) -> Maybe Natural.Natural -> (Deck.Deck -> Map.Map Printing.Printing Natural.Natural) -> Deck.Deck -> [Violation.Violation]
copiesAtMost restricted limit counted deck =
  let tallies = Map.fromListWith (\(_, new) (card, old) -> (card, old + new)) [(Face.name (Card.combined card), (card, count)) | (printing, count) <- Map.toList (counted deck), let card = Printing.card printing, not (basicLand card)]
   in [Violation.TooManyCopies written count most | (written, (card, count)) <- Map.toList tallies, Just most <- [limitOf card], count > most]
  where
    limitOf card
      | restricted card = Just 1
      | otherwise = case Face.deckLimit (Card.combined card) of
          Nothing -> limit
          Just (DeckLimit.UpTo most) -> Just most
          Just DeckLimit.AnyNumber -> Nothing

-- CR 100.4a: a maximum sideboard size.
sideboardAtMost :: Natural.Natural -> Deck.Deck -> [Violation.Violation]
sideboardAtMost most deck =
  let size = sum (Deck.sideboard deck)
   in [Violation.SideboardTooLarge size most | size > most]

-- A card's standing in one pool: Nothing for a card the table does not have,
-- and Just Nothing for one the pool does not have, which is not legal there.
poolStanding :: LegalityTable.LegalityTable -> Text.Text -> Card.Type.Card -> Maybe (Maybe Legality.Legality)
poolStanding table pool card = fmap (Map.lookup pool) (Map.lookup (Ingest.cardKey card) (LegalityTable.cards table))

-- Each card of deck and sideboard the pool bans, does not have, or the table
-- cannot say of. A restricted card is copiesAtMost's.
inPool :: (Card.Type.Card -> Maybe (Maybe Legality.Legality)) -> Deck.Deck -> [Violation.Violation]
inPool standing = foldMap judge . cardsIn
  where
    judge (written, card) = case standing card of
      Nothing -> [Violation.NoLegalityData written]
      Just Nothing -> [Violation.NotLegalCard written]
      Just (Just Legality.Banned) -> [Violation.BannedCard written]
      Just (Just Legality.Restricted) -> []
      Just (Just Legality.Legal) -> []

-- CR 407.3: no ante card in deck or sideboard of a game not played for ante,
-- with a pool or without one.
anteCards :: GameSettings.GameSettings -> Deck.Deck -> [Violation.Violation]
anteCards settings deck = [Violation.AnteCard written | (written, card) <- cardsIn deck, Ante.barred settings (Card.combined card)]

-- CR 315.1: no conspiracy card in constructed play, sideboard included.
conspiracies :: Deck.Deck -> [Violation.Violation]
conspiracies deck = [Violation.ConspiracyCard written | (written, card) <- cardsIn deck, Set.member CardType.Conspiracy (TypeLine.types (Face.typeLine (Card.combined card)))]

-- Each card of deck and sideboard once, by name.
cardsIn :: Deck.Deck -> [(CardName.CardName, Card.Type.Card)]
cardsIn deck = Map.toList (Map.fromList [(Face.name (Card.combined card), card) | printing <- Map.keys (bothSections deck), let card = Printing.card printing])

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
  Violation.TooManyCopies written count limit -> show count <> " copies of " <> spelled written <> ", more than " <> show limit
  Violation.SideboardTooLarge size most -> show size <> " cards in the sideboard, more than " <> show most
  Violation.BannedCard written -> spelled written <> " is banned"
  Violation.NotLegalCard written -> spelled written <> " is not legal"
  Violation.NoLegalityData written -> spelled written <> " has no legality data"
  Violation.AnteCard written -> spelled written <> " is an ante card in a game not for ante"
  Violation.ConspiracyCard written -> spelled written <> " is a conspiracy card in constructed play"
  where
    spelled = Text.unpack . CardName.unwrap

-- | The check of the shipped format a name means, against the shipped table:
-- "constructed" and "limited" for the rules alone, "vintage" and the like with
-- a pool.
named :: Text.Text -> IO (Either Text.Text (GameSettings.GameSettings -> Deck.Deck -> [Violation.Violation]))
named name = do
  root <- formatsRoot
  found <- loadFormat root name
  case found of
    Nothing -> pure (Left (Text.pack "not a format: " <> name))
    Just (Left problem) -> pure (Left (name <> Text.pack ": " <> problem))
    Just (Right format) -> do
      table <- tablePath >>= loadTable
      pure (fmap (\loaded settings -> check loaded settings format) table)

-- | Where the shipped legality table is.
tablePath :: IO FilePath
tablePath = Paths.getDataFileName "legalities.json"

-- | The legality table a file holds.
loadTable :: FilePath -> IO (Either Text.Text LegalityTable.LegalityTable)
loadTable path = fmap (decodeWith LegalityTable.Codec.codec) (ByteString.readFile path)

-- | The directory the shipped formats are in, one file each.
formatsRoot :: IO FilePath
formatsRoot = Paths.getDataFileName "formats"

-- | The format a name means, the file of that name under a formats root:
-- Nothing when there is none.
loadFormat :: FilePath -> Text.Text -> IO (Maybe (Either Text.Text Format.Format))
loadFormat root name
  | Text.null name || Text.any (\c -> not (Char.isAsciiLower c || Char.isDigit c)) name = pure Nothing
  | otherwise = do
      let path = root <> "/" <> Text.unpack name <> ".json"
      exists <- Directory.doesFileExist path
      if exists
        then fmap (Just . decodeWith Format.Codec.codec) (ByteString.readFile path)
        else pure Nothing

decodeWith :: Codec.Codec a -> ByteString.ByteString -> Either Text.Text a
decodeWith codec bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Codec.decode codec

-- | A table as data/legalities.json holds it: Parun.Codec.LegalityTable's
-- wire, one card a line so a ban announcement's diff names the cards.
renderTable :: LegalityTable.LegalityTable -> Text.Text
renderTable table =
  let quoted = Common.render . Codec.encode Common.text
      card (key, pools) = Text.pack "    " <> quoted key <> Text.pack ": " <> Common.render (Codec.encode (Common.textMap id Right Legality.Codec.codec) pools)
   in Text.unlines
        [ Text.pack "{",
          Text.pack "  \"date\": " <> quoted (LegalityTable.date table) <> Text.pack ",",
          Text.pack "  \"cards\": {",
          Text.intercalate (Text.pack ",\n") (fmap card (Map.toAscList (LegalityTable.cards table))),
          Text.pack "  }",
          Text.pack "}"
        ]
