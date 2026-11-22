-- | The web client's first screen: what to do, drawn before any data has
-- loaded (#9075). A game left for here comes first, to resume; then the
-- ways to play, to build a deck and to watch a replay; then the games
-- finished lately and the decks saved in this browser; then the legal
-- notice.
module Parun.Web.Home where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.Color as Color
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Card as Card
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.History as History
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Mana as Mana
import qualified Parun.Web.Replay as Replay

data Home = MkHome
  { -- | The game left for here, to resume.
    paused :: Maybe Paused,
    -- | A new game chosen while one is paused, asked about first.
    confirming :: Maybe Choice,
    -- | Why something the screen waits on did not load.
    problem :: Maybe Text.Text,
    -- | The decks saved in this browser, as read so far.
    saved :: [Deck],
    -- | The games finished in this browser, newest first (Parun.Web.History).
    finished :: [History.Game]
  }
  deriving (Eq, Show)

-- | Where a game left for the home screen stands, as the question waiting
-- showed it.
data Paused = MkPaused
  { -- | Nothing where the question showed no board.
    turn :: Maybe Natural,
    -- | Each seat's name and life, in turn order.
    lives :: [(Text.Text, Integer)]
  }
  deriving (Eq, Show)

-- | A saved deck as the home screen lists it.
data Deck = MkDeck
  { -- | Its name (Lobby.deckName).
    name :: Text.Text,
    -- | Its colours, empty until read.
    colours :: Set.Set Color.Color,
    -- | How many cards its main deck holds, once read.
    size :: Maybe Natural
  }
  deriving (Eq, Show)

-- | Where the home screen leads.
data Choice
  = -- | The lobby, with this deck yours if any.
    MkPlayBots (Maybe Text.Text)
  | -- | The lobby, with a bot's seat given to a player invited by link.
    MkPlayFriend
  | -- | The deck builder, on a new deck.
    MkBuildDeck
  | -- | The deck builder, on this saved deck.
    MkEditDeck Text.Text
  | -- | The Decks screen, with this deck selected if any.
    MkBrowseDecks (Maybe Text.Text)
  | MkReplays
  | -- | This replay, from its start.
    MkWatch Replay.Source
  | -- | Plays on by hand from this position of this replay.
    MkPlayOn Replay.Source Natural
  | -- | The lobby, on these decks: yours, then each bot's.
    MkPlayAgain (NonEmpty.NonEmpty Text.Text)
  deriving (Eq, Show)

-- | What a tap on the home screen does.
data Event
  = MkChoose Choice
  | MkResume
  | -- | Concedes the paused game for the choice asked about.
    MkConfirm
  | MkCancel
  deriving (Eq, Show)

-- | What the home screen does after an event.
data Outcome
  = -- | Shows this home screen.
    MkStay Home
  | -- | Leaves for this choice, conceding any paused game for a new one.
    MkGo Choice
  | -- | Returns to the paused game.
    MkBack
  deriving (Eq, Show)

-- | Nothing loaded, no game paused.
new :: Home
new = MkHome Nothing Nothing Nothing [] []

-- | A game paused at the board the question waiting showed, if any.
pausedOn :: Maybe PlayerView.PlayerView -> Paused
pausedOn board = case board of
  Nothing -> MkPaused Nothing []
  Just b -> MkPaused (Just (PlayerView.turn b)) [(if SeatView.player s == PlayerView.viewer b then Text.pack "You" else Wording.player (SeatView.player s), SeatView.life s) | s <- PlayerView.seats b]

-- | Whether a choice starts a game, and so concedes a paused one.
starts :: Choice -> Bool
starts choice = case choice of
  MkPlayBots _ -> True
  MkPlayFriend -> True
  MkBuildDeck -> False
  MkEditDeck _ -> False
  MkBrowseDecks _ -> False
  MkReplays -> False
  MkWatch _ -> False
  MkPlayOn _ _ -> True
  MkPlayAgain _ -> True

-- | What a tap on the top bar's section means here.
navigate :: Bar.Section -> Event
navigate section = case section of
  Bar.MkHome -> MkCancel
  Bar.MkPlay -> MkChoose (MkPlayBots Nothing)
  Bar.MkDecks -> MkChoose (MkBrowseDecks Nothing)
  Bar.MkReplays -> MkChoose MkReplays

update :: Event -> Home -> Outcome
update event home = case event of
  MkChoose choice
    | Maybe.isJust (paused home) && starts choice -> MkStay home {confirming = Just choice}
    | otherwise -> MkGo choice
  MkResume -> MkBack
  MkConfirm -> maybe (MkStay home) MkGo (confirming home)
  MkCancel -> MkStay home {confirming = Nothing}

view :: Home -> Html.Html Event
view home =
  Element.el "main" ["home"] [] $
    [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (problem home)]
      <> case confirming home of
        Just choice ->
          [ Element.el
              "section"
              ["panel", "confirm"]
              [Element.attr "role" (Text.pack "alertdialog"), Element.attr "aria-label" (Text.pack "Concede the game in progress?")]
              [ Element.el "p" [] [] [Element.string "Starting a new game concedes the one in progress."],
                Element.el
                  "div"
                  ["choices"]
                  []
                  [ Element.button ["primary"] MkConfirm [Element.string (confirmLabel choice)],
                    Element.button [] MkCancel [Element.string "Cancel"]
                  ]
              ]
          ]
        Nothing ->
          foldMap (pure . pausedView) (paused home)
            <> [ Element.el
                   "nav"
                   ["tiles"]
                   [Element.attr "aria-label" (Text.pack "What to do")]
                   [ tile ["lead" | Maybe.isNothing (paused home)] (MkPlayBots Nothing) "Play a bot" "Pick a deck for each seat and start.",
                     tile [] MkPlayFriend "Play a friend" "Invite someone in another browser with a link.",
                     tile [] MkBuildDeck "Build a deck" "Start from a blank list, then save it to play.",
                     tile [] MkReplays "Replays" "Watch your games again, or the scenarios the rules are tested on."
                   ]
               ]
            <> [recentView (take recentCount (finished home)) | not (null (finished home))]
            <> [decksView (saved home) | not (null (saved home))]
      <> [legal]
  where
    confirmLabel choice = case choice of
      MkPlayOn _ _ -> "Concede and play on"
      _ -> "Concede and play"
    tile classes choice title said =
      Element.button
        ("tile" : classes)
        (MkChoose choice)
        [Element.el "strong" [] [] [Element.string title], Element.el "span" ["muted"] [] [Element.string said]]

-- | The game in progress: its turn and life totals, and the way back to it.
pausedView :: Paused -> Html.Html Event
pausedView game =
  Element.el
    "section"
    ["panel", "in-progress"]
    [Element.attr "aria-label" (Text.pack "Game in progress")]
    [ Element.el
        "div"
        ["where"]
        []
        [ Element.el "span" ["eyebrow"] [] [Element.string "Game in progress"],
          Element.el "h1" [] [] [Element.string (maybe "Waiting for you" (\n -> "Turn " <> show n) (turn game))],
          Element.el "p" ["muted"] [] [Element.string "Starting another game concedes this one."]
        ],
      Element.el
        "dl"
        ["lives"]
        []
        [Element.el "div" ["seat-life"] [] [Element.el "dt" ["eyebrow"] [] [Element.text who], Element.el "dd" [] [] [Element.string (show life)]] | (who, life) <- lives game],
      Element.button ["primary", "resume"] MkResume [Element.string "Resume"]
    ]

-- | How many of the games finished lately the home screen lists.
recentCount :: Int
recentCount = 5

-- | The games finished lately, each to watch again, and the way to every one.
recentView :: [History.Game] -> Html.Html Event
recentView games =
  Element.el
    "section"
    ["panel", "recent-games"]
    []
    [ Element.el "div" ["panel-head"] [] [Element.el "h2" [] [] [Element.string "Recent games"], Element.button ["quiet"] (MkChoose MkReplays) [Element.string "All games"]],
      Element.el "ul" ["game-rows"] [] [History.row game [Element.button ["quiet"] (MkChoose (MkWatch (Replay.MkPlayed (History.key game)))) [Element.string "Watch"]] | game <- games]
    ]

-- | The decks saved in this browser, each opening the Decks screen on it,
-- and each with the way to edit it.
decksView :: [Deck] -> Html.Html Event
decksView decks =
  Element.el
    "section"
    ["panel", "your-decks"]
    []
    [ Element.el "h2" [] [] [Element.string "Your decks"],
      Element.el "ul" ["deck-rows"] [] (fmap row decks)
    ]
  where
    row deck =
      Element.el
        "li"
        []
        []
        [ Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Card.letterUpper c)) | c <- Set.toList (colours deck)],
          Element.button ["quiet", "deck-row-name"] (MkChoose (MkBrowseDecks (Just (name deck)))) [Element.text (snd (DeckBrowser.describe (name deck)))],
          Element.el "span" ["muted"] [] [Element.string (maybe "" (\n -> show n <> " cards") (size deck))],
          Element.button ["quiet"] (MkChoose (MkEditDeck (name deck))) [Element.string "Edit"]
        ]

-- | The Fan Content Policy's notice, and where Parun's source is.
legal :: Html.Html msg
legal =
  Element.el
    "footer"
    ["legal"]
    []
    [ Element.el
        "p"
        []
        []
        [ Element.string "Parun is unofficial Fan Content permitted under the ",
          link "https://company.wizards.com/en/legal/fancontentpolicy" "Fan Content Policy",
          Element.string ". Not approved/endorsed by Wizards. Portions of the materials used are property of Wizards of the Coast. \x00a9Wizards of the Coast LLC. Magic: The Gathering is a trademark of Wizards of the Coast LLC; Universes Beyond names belong to their respective owners."
        ],
      Element.el "p" [] [] [link "https://github.com/parun-mtg/parun" "Parun on GitHub"]
    ]
  where
    link href said = Element.el "a" [] [Element.attr "href" (Text.pack href), Element.attr "target" (Text.pack "_blank"), Element.attr "rel" (Text.pack "noopener")] [Element.string said]
