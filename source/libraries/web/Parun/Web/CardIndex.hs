-- | The deck builder's card search: a slim entry per card a deck may hold,
-- and the query that filters and orders them.
module Parun.Web.CardIndex where

import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Engine.Quantity as Quantity
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.Supertype as Supertype
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.Lobby as Lobby

-- | A card as the builder searches and shows it.
data Entry = MkEntry
  { name :: CardName.CardName,
    -- | Each face's name, in printed order.
    faceNames :: [Text.Text],
    -- | Its cost as its faces combine (CR 709.4b), or else its first face's.
    cost :: Maybe ManaCost.ManaCost,
    -- | Its mana value, likewise (CR 202.3, 709.4b).
    value :: Integer,
    colours :: Set.Set Color.Color,
    -- | Every face's card types.
    types :: Set.Set CardType.CardType,
    -- | The first face's type line, as words.
    typeLine :: Text.Text,
    -- | Each face's name and its rules text, a line each.
    rules :: [(Text.Text, [Text.Text])],
    -- | Every face's name, type line and rules text, case-folded, to match.
    folded :: Text.Text,
    -- | Whether the card is a basic land, which every card pool admits
    -- (CR 100.2b, 205.4c).
    basic :: Bool,
    -- | The choices the card makes as its deck is built, in each section of
    -- a list that reads one (Parun.DeckList.makes).
    choices :: Set.Set (DeckList.Section, DeckList.Choice)
  }
  deriving (Eq, Show)

-- | What the cards are filtered by; an empty field filters nothing.
data Query = MkQuery
  { text :: Text.Text,
    -- | The colours a card's must lie within.
    wanted :: Set.Set Color.Color,
    -- | The card types a card must have one of.
    kinds :: Set.Set CardType.CardType,
    -- | The mana value a card must have, 7 meaning 7 and up.
    valued :: Maybe Integer,
    -- | The names, case-folded, a card's must be among; a basic land is
    -- always kept.
    among :: Maybe (Set.Set Text.Text),
    -- | The supplementary deck a card must fit (pileOf); nothing for a
    -- traditional card.
    pile :: Maybe SideDeck.SideDeck
  }
  deriving (Eq, Show)

-- | A query filtering nothing.
everything :: Query
everything = MkQuery Text.empty Set.empty Set.empty Nothing Nothing Nothing

-- | How many more results each showing adds, and so how many are shown at
-- first.
cap :: Int
cap = 60

-- | The card as the builder indexes it; nothing for a card no deck the
-- builder edits holds: a synthetic one, a vanguard or scheme card (CR
-- 313.1, 314.1), a dungeon (CR 309.2) or a conspiracy (CR 315.3).
entryOf :: Card.Card -> Maybe Entry
entryOf c
  | Card.synthetic c || any (`Set.member` kindsOf) outside = Nothing
  | otherwise =
      Just
        MkEntry
          { -- As a deck list writes it (Parun.DeckList.written).
            name = CardName.MkCardName (Text.intercalate (Text.pack " // ") (NonEmpty.toList (fmap (CardName.unwrap . Face.name) (EngineCard.combinedFaces c)))),
            faceNames = named,
            cost = Face.manaCost (EngineCard.combined c),
            value = Quantity.manaValueOf (EngineCard.combined c),
            colours = Lobby.cardColours c,
            types = kindsOf,
            typeLine = typeWords (Face.typeLine front),
            rules = [(CardName.unwrap (Face.name face), foldMap Text.lines (Face.oracleText face)) | face <- faces, Maybe.isJust (Face.oracleText face)],
            folded = Text.toCaseFold (Text.unlines (named <> fmap (typeWords . Face.typeLine) faces <> said)),
            choices = Set.fromList [(section, choice) | section <- [DeckList.Main, DeckList.Side, DeckList.Commander], choice <- [minBound .. maxBound], DeckList.makes section choice c],
            basic = Set.member Supertype.Basic (TypeLine.supertypes (Face.typeLine front)) && Set.member CardType.Land (TypeLine.types (Face.typeLine front))
          }
  where
    faces = NonEmpty.toList (Card.faces c)
    front = NonEmpty.head (Card.faces c)
    named = fmap (CardName.unwrap . Face.name) faces
    said = foldMap (foldMap Text.lines . Face.oracleText) faces
    kindsOf = foldMap (TypeLine.types . Face.typeLine) faces
    outside = [CardType.Conspiracy, CardType.Dungeon, CardType.Scheme, CardType.Vanguard]

-- | The supplementary deck a card belongs in: a plane or phenomenon card's
-- planar deck (CR 901.3); nothing for a traditional card.
pileOf :: Entry -> Maybe SideDeck.SideDeck
pileOf e
  | Set.member CardType.Plane (types e) || Set.member CardType.Phenomenon (types e) = Just SideDeck.Planar
  | otherwise = Nothing

-- | A type line as words: supertypes and card types, then a dash and the
-- subtypes, if any.
typeWords :: TypeLine.TypeLine -> Text.Text
typeWords line =
  let shown :: (Show a) => a -> Text.Text
      shown = Text.pack . show
      front = fmap shown (Set.toList (TypeLine.supertypes line)) <> fmap shown (Set.toList (TypeLine.types line))
   in Text.unwords (front <> (if null (TypeLine.subtypes line) then [] else Text.pack "\x2014" : fmap shown (TypeLine.subtypes line)))

-- | How many entries match, and the first `most` of them: those whose name
-- starts with the text, then those with it in a face's name, then those
-- with it anywhere else, each in the order given, which is by name.
search :: Int -> Query -> Seq.Seq Entry -> (Int, Seq.Seq Entry)
search most query entries =
  let needle = Text.toCaseFold (Text.strip (text query))
      rank e
        | Text.isPrefixOf needle (Text.toCaseFold (CardName.unwrap (name e))) = Just (0 :: Int)
        | any (Text.isInfixOf needle . Text.toCaseFold) (faceNames e) = Just 1
        | Text.isInfixOf needle (folded e) = Just 2
        | otherwise = Nothing
      within e = Set.null (wanted query) || Set.isSubsetOf (colours e) (wanted query)
      ofKind e = Set.null (kinds query) || not (Set.disjoint (types e) (kinds query))
      pooled e = basic e || maybe True (Set.member (Text.toCaseFold (CardName.unwrap (name e)))) (among query)
      atValue e = maybe True (\v -> if v >= 7 then value e >= 7 else value e == v) (valued query)
      -- Stable, so each rank keeps the entries' own order.
      ranked = Seq.sortOn fst (Seq.fromList [(r, e) | e <- Foldable.toList entries, pileOf e == pile query, pooled e, within e, ofKind e, atValue e, Just r <- [rank e]])
   in (Seq.length ranked, fmap snd (Seq.take most ranked))
