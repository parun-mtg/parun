-- | The web client's first screen: what to do, drawn before any data has
-- loaded (#9075). Playing comes first, replays second, and a game left
-- for here can be resumed.
module Parun.Web.Home where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

data Home = MkHome
  { -- | The card files loaded, once known.
    cards :: Maybe Natural,
    -- | Whether a game was left for here, to resume.
    paused :: Bool,
    -- | A new game chosen while one is paused, asked about first.
    confirming :: Maybe Choice,
    -- | Why something the screen waits on did not load.
    problem :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | Where the home screen leads.
data Choice
  = MkPlayBots
  | MkReplays
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
new = MkHome Nothing False Nothing Nothing

-- | Whether a choice starts a game, and so concedes a paused one.
starts :: Choice -> Bool
starts choice = case choice of
  MkPlayBots -> True
  MkReplays -> False

update :: Event -> Home -> Outcome
update event home = case event of
  MkChoose choice
    | paused home && starts choice -> MkStay home {confirming = Just choice}
    | otherwise -> MkGo choice
  MkResume -> MkBack
  MkConfirm -> maybe (MkStay home) MkGo (confirming home)
  MkCancel -> MkStay home {confirming = Nothing}

-- | The logo, the name, and the tagline, with the card count once known.
hero :: Maybe Natural -> Html.Html msg
hero count =
  Element.el
    "header"
    ["hero"]
    []
    [ Element.el "h1" [] [] [Element.el "img" ["logo"] [Element.attr "src" (Text.pack "./icon.svg"), Element.attr "alt" Text.empty, Element.attr "width" (Text.pack "40"), Element.attr "height" (Text.pack "40")] [], Element.string "Parun"],
      Element.el "p" ["tagline"] [] [Element.string ("A Magic rules engine, running in your browser." <> foldMap (\n -> " " <> show n <> " cards loaded.") count)]
    ]

view :: Home -> Html.Html Event
view home =
  Element.el "main" ["home"] [] $
    [hero (cards home)]
      <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (problem home)]
      <> case confirming home of
        Just choice ->
          [ Element.el
              "section"
              ["confirm"]
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
          [ Element.el "nav" ["choices"] [Element.attr "aria-label" (Text.pack "What to do")] $
              [Element.button ["primary", "resume"] MkResume [Element.string "Resume game"] | paused home]
                <> [ Element.button (["play"] <> ["primary" | not (paused home)]) (MkChoose MkPlayBots) [Element.string "Play against bots"],
                     Element.button ["replays"] (MkChoose MkReplays) [Element.string "Replays"]
                   ]
          ]
  where
    confirmLabel choice = case choice of
      MkPlayBots -> "Concede and play"
      MkReplays -> "Concede"
