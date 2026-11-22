-- | The top bar of every screen but the game's (#9421): the logo and the
-- name, the client's sections, and the settings, which the game's turn bar
-- carries instead.
module Parun.Web.Bar where

import qualified Data.Text as Text
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon

-- | A section of the client the bar leads to.
data Section
  = MkHome
  | -- | The lobby, where a game is set up.
    MkPlay
  | -- | The decks screen (Parun.Web.Decks), and the builder from it.
    MkDecks
  | -- | The scenarios' replays.
    MkReplays
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | A section's name on the bar.
title :: Section -> String
title section = case section of
  MkHome -> "Home"
  MkPlay -> "Play"
  MkDecks -> "Decks"
  MkReplays -> "Replays"

-- | The bar, the section shown marked as the current page; a tap on a
-- section sends it.
view :: Maybe Section -> Html.Html Section
view current =
  Element.el
    "header"
    ["topbar"]
    []
    [ Element.el
        "div"
        ["brand"]
        []
        [ Element.el "img" ["logo"] [Element.attr "src" (Text.pack "./icon.svg"), Element.attr "alt" Text.empty, Element.attr "width" (Text.pack "34"), Element.attr "height" (Text.pack "34")] [],
          Element.el "span" ["wordmark"] [] [Element.string "Parun"]
        ],
      Element.el "nav" ["sections"] [Element.attr "aria-label" (Text.pack "Sections")] (fmap link [minBound .. maxBound]),
      settingsButton Icon.MkGear "Settings" Nothing Nothing
    ]
  where
    link section =
      Element.el
        "button"
        ["section"]
        ([Element.attr "type" (Text.pack "button"), Element.on "click" section] <> [Element.attr "aria-current" (Text.pack "page") | current == Just section])
        [Element.string (title section)]

-- | A screen under the bar, its section taps sent as `nav` makes them.
framed :: (Section -> msg) -> Maybe Section -> Html.Html msg -> Html.Html msg
framed nav current screen = Element.el "div" ["framed"] [] [fmap nav (view current), screen]

-- | The button page.js opens its settings menu from (index.html's
-- #settings), with this icon and name, carrying the game's Home and
-- Concede events where given (page.js offers each in the menu).
settingsButton :: Icon.Icon -> String -> Maybe msg -> Maybe msg -> Html.Html msg
settingsButton i name home concede =
  Element.el
    "button"
    ["settings-button"]
    ( [Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (Text.pack name), Element.attr "aria-haspopup" (Text.pack "true"), Element.attr "aria-controls" (Text.pack "settings"), Element.attr "data-settings" Text.empty]
        <> foldMap (\event -> [Element.attr "data-home" Text.empty, Element.on "home" event]) home
        <> foldMap (\event -> [Element.attr "data-concede" Text.empty, Element.on "concede" event]) concede
    )
    [Icon.icon [] i]
