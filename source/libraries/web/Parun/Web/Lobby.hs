-- | The web client's first screen: the match-up being set, your deck against
-- the bot's, and every deck, grouped by set and filtered by what is typed.
module Parun.Web.Lobby where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana

data Lobby = MkLobby
  { -- | The card files the registry holds.
    cardFiles :: Natural,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | The player's deck, once chosen.
    mine :: Maybe Text.Text,
    -- | The bot's deck, once chosen; a random one otherwise.
    theirs :: Maybe Text.Text,
    -- | Which deck a tapped deck becomes.
    choosing :: Slot,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | Each deck's colours, by its name (colours).
    palette :: Map.Map Text.Text (Set.Set Color.Color)
  }
  deriving (Eq, Show)

-- | A side of the match-up.
data Slot
  = MkMine
  | MkTheirs
  deriving (Eq, Show)

-- | What a tap or a keystroke on the lobby does.
data Event
  = -- | Makes this deck the chosen slot's.
    MkPick Text.Text
  | -- | Chooses a deck for this slot next.
    MkChoose Slot
  | -- | Filters the decks by the text typed.
    MkSearch
  | -- | A random deck for the slot being chosen; the page picks it.
    MkRandom
  | -- | Starts the game.
    MkStart
  deriving (Eq, Show)

-- | A lobby holding these decks, nothing chosen yet.
new :: Natural -> Seq.Seq Text.Text -> Lobby
new files names = MkLobby files names Nothing Nothing MkMine Text.empty Map.empty

-- | A card's colours as a deck shows them: those of the mana symbols in
-- its faces' costs.
cardColours :: Card.Card -> Set.Set Color.Color
cardColours card = foldMap (foldMap costColours . Face.manaCost) (Card.faces card)

-- | The colours a cost's symbols name (CR 105.2, 202.2).
costColours :: ManaCost.ManaCost -> Set.Set Color.Color
costColours cost =
  let ofType t = case t of
        ManaType.Colored c -> Set.singleton c
        ManaType.Colorless -> Set.empty
      symbol sym = case sym of
        ManaSymbol.Generic _ -> Set.empty
        ManaSymbol.OfType t -> ofType t
        ManaSymbol.Hybrid h -> Set.union (ofType (Hybrid.left h)) (ofType (Hybrid.right h))
        ManaSymbol.MonocoloredHybrid t -> ofType t
        ManaSymbol.Phyrexian c -> Set.singleton c
        ManaSymbol.HybridPhyrexian h -> Set.fromList [HybridPhyrexian.left h, HybridPhyrexian.right h]
        ManaSymbol.Snow -> Set.empty
        ManaSymbol.Variable -> Set.empty
   in foldMap symbol (ManaCost.unwrap cost)

-- | A deck list's colours: those of each card a line names ("4 Forest",
-- "1 Shock (M19) 156"), by `colours`; other lines skipped.
deckColours :: (CardName.CardName -> Set.Set Color.Color) -> Text.Text -> Set.Set Color.Color
deckColours colours contents =
  let named line = do
        let (count, rest) = Text.span Char.isDigit (Text.strip line)
        Monad.guard (not (Text.null count))
        pure (CardName.MkCardName (Text.strip (fst (Text.breakOn (Text.pack " (") rest))))
   in foldMap colours (Set.fromList (Maybe.mapMaybe named (Text.lines contents)))

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

-- | The lobby after an event, the value typed being `typed`. Picking your
-- deck moves on to the bot's while that is unchosen.
update :: Event -> Text.Text -> Lobby -> Lobby
update event typed lobby = case event of
  MkPick name
    | Foldable.notElem name (decks lobby) -> lobby
    | choosing lobby == MkMine -> lobby {mine = Just name, choosing = if Maybe.isNothing (theirs lobby) then MkTheirs else MkMine}
    | otherwise -> lobby {theirs = Just name}
  MkChoose slot -> lobby {choosing = slot}
  MkSearch -> lobby {search = typed}
  MkRandom -> lobby
  MkStart -> lobby

-- | A deck's set and its own name: "sets/m12/entangling-webs" is set "M12",
-- deck "Entangling Webs", and a set's whole card list is "Every card".
describe :: Text.Text -> (Text.Text, Text.Text)
describe name =
  let parts = Text.splitOn (Text.pack "/") name
      (group, leaf) = case reverse parts of
        l : s : _ -> (Text.toUpper s, l)
        [l] -> (Text.pack "Decks", l)
        [] -> (Text.pack "Decks", name)
   in (group, if leaf == Text.pack "all" then Text.pack "Every card" else titled leaf)

-- | "entangling-webs" as "Entangling Webs".
titled :: Text.Text -> Text.Text
titled = Text.unwords . fmap capital . Text.split (\c -> c == '-' || c == '_')
  where
    capital w = case Text.uncons w of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> w

-- | The deck as the match-up names it: its own name and its set.
label :: Text.Text -> Text.Text
label name = let (group, deck) = describe name in deck <> Text.pack " (" <> group <> Text.pack ")"

-- | Whether the deck matches what is typed, by its path or its name, any case.
matches :: Text.Text -> Text.Text -> Bool
matches typed name =
  let needle = Text.toCaseFold (Text.strip typed)
   in Text.null needle || Text.isInfixOf needle (Text.toCaseFold name) || Text.isInfixOf needle (Text.toCaseFold (label name))

view :: Lobby -> Html.Html Event
view lobby =
  let shown = filter (matches (search lobby)) (Foldable.toList (decks lobby))
      grouped = Map.toList (Map.fromListWith (flip (<>)) [(fst (describe name), [name]) | name <- shown])
      -- A set with nothing but its whole list joins the others like it,
      -- each named by its set.
      lone names = case names of
        [name] -> Text.isSuffixOf (Text.pack "/all") name
        _ -> False
      groups = [g | g@(_, names) <- grouped, not (lone names)]
      wholes = concat [names | (_, names) <- grouped, lone names]
      slot which title chosen fallback =
        Element.button
          (["slot"] <> ["choosing" | choosing lobby == which])
          (MkChoose which)
          [ Element.el "span" ["slot-title"] [] [Element.string title],
            Element.el "span" ["slot-deck"] [] [Element.text (maybe (Text.pack fallback) label chosen)]
          ]
      deck name = deckAs (snd (describe name)) name
      deckAs title name =
        let picked = Just name == (if choosing lobby == MkMine then mine lobby else theirs lobby)
            pips = Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList (Map.findWithDefault Set.empty name (palette lobby))]
         in (name, Element.button (["deck"] <> ["picked" | picked]) (MkPick name) [Element.el "span" ["deck-name"] [] [Element.text title], pips])
   in Element.el
        "main"
        ["lobby"]
        []
        [ Element.el
            "header"
            ["hero"]
            []
            [ Element.el "h1" [] [] [Element.string "Parun"],
              Element.el "p" ["tagline"] [] [Element.string ("A Magic rules engine, running in your browser. " <> show (cardFiles lobby) <> " cards loaded.")]
            ],
          Element.el
            "section"
            ["matchup"]
            [Element.attr "aria-label" (Text.pack "Match-up")]
            [ slot MkMine "You" (mine lobby) "Choose a deck",
              Element.el "span" ["versus"] [] [Element.string "vs"],
              slot MkTheirs "Bot" (theirs lobby) "Random deck",
              Element.el
                "button"
                ["primary", "start"]
                ([Element.attr "type" (Text.pack "button"), Element.on "click" MkStart] <> [Element.attr "disabled" Text.empty | Maybe.isNothing (mine lobby)])
                [Element.string "Start game"]
            ],
          Element.el
            "section"
            ["deck-picker"]
            []
            ( [ Element.el
                  "div"
                  ["deck-tools"]
                  []
                  [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
                    Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
                    Element.button ["random"] MkRandom [Element.string "Random"]
                  ],
                Element.el "p" ["choosing-for"] [] [Element.string (if choosing lobby == MkMine then "Choose your deck." else "Choose the bot's deck.")],
                Element.keyed "div" ["sets"] [] $
                  [(group, Element.el "section" ["set"] [] [Element.el "h2" [] [] [Element.text group], Element.keyed "div" ["deck-list"] [] (fmap deck names)]) | (group, names) <- groups]
                    <> [(Text.pack " whole", Element.el "section" ["set", "wholes"] [] [Element.el "h2" [] [] [Element.string "Whole sets"], Element.keyed "div" ["deck-list"] [] [deckAs (fst (describe name)) name | name <- wholes]]) | not (null wholes)]
              ]
                <> [Element.el "p" ["empty"] [] [Element.string "No deck matches."] | null shown]
            )
        ]
