-- | The web client's first screen: the match-up being set, your deck against
-- the bot's, and every deck, grouped by set and filtered by what is typed.
module Parun.Web.Lobby where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

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
    search :: Text.Text
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
new files names = MkLobby files names Nothing Nothing MkMine Text.empty

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
      groups = Map.toList (Map.fromListWith (flip (<>)) [(fst (describe name), [name]) | name <- shown])
      slot which title chosen fallback =
        Element.button
          (["slot"] <> ["choosing" | choosing lobby == which])
          (MkChoose which)
          [ Element.el "span" ["slot-title"] [] [Element.string title],
            Element.el "span" ["slot-deck"] [] [Element.text (maybe (Text.pack fallback) label chosen)]
          ]
      deck name =
        let picked = Just name == (if choosing lobby == MkMine then mine lobby else theirs lobby)
         in (name, Element.button (["deck"] <> ["picked" | picked]) (MkPick name) [Element.text (snd (describe name))])
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
                Element.keyed "div" ["sets"] [] [(group, Element.el "section" ["set"] [] [Element.el "h2" [] [] [Element.text group], Element.keyed "div" ["deck-list"] [] (fmap deck names)]) | (group, names) <- groups]
              ]
                <> [Element.el "p" ["empty"] [] [Element.string "No deck matches."] | null shown]
            )
        ]
