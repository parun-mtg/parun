-- | The screen a link from a host opens (#9185): the guest picks their own
-- deck, sends back the reply code it makes, and once connected sees the
-- whole match-up and says when they are ready.
module Parun.Web.Join where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby

data Join = MkJoin
  { -- | Whether the deck lists are still to read.
    reading :: Bool,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | The deck chosen.
    picked :: Maybe Text.Text,
    -- | The code to send the host, once made.
    reply :: Maybe Text.Text,
    -- | Whether the host's page is connected.
    connected :: Bool,
    -- | The match-up as the host last sent it.
    places :: [Link.Place],
    -- | Why the connection failed or ended.
    problem :: Maybe Text.Text
  }
  deriving (Eq, Show)

data Event
  = -- | Makes this deck yours.
    MkPick Text.Text
  | -- | Filters the decks by the text typed.
    MkSearch
  | -- | Makes the reply code; the shell does it.
    MkMakeReply
  | -- | Toggles whether you are ready; the shell tells the host.
    MkReady
  deriving (Eq, Show)

-- | A join screen whose decks are still to read.
new :: Join
new = MkJoin True Seq.empty Text.empty Nothing Nothing False [] Nothing

-- | The screen after an event, the value typed being `typed`.
update :: Event -> Text.Text -> Join -> Join
update event typed screen = case event of
  MkPick name | Foldable.elem name (decks screen) -> screen {picked = Just name}
  MkSearch -> screen {search = typed}
  _ -> screen

-- | The screen a Rematch returns to, still connected: the match-up just
-- played, nobody ready, until the host tells it afresh.
rematched :: Join -> Join
rematched screen = screen {connected = True, problem = Nothing, places = fmap (\p -> p {Link.ready = False <$ Link.ready p}) (places screen)}

-- | Whether the host last said you are ready.
isReady :: Join -> Bool
isReady screen = Maybe.fromMaybe False (List.find ((== Text.pack "You") . Link.who) (places screen) >>= Link.ready)

view :: Join -> Html.Html Event
view screen =
  let shown = filter (Lobby.matches (search screen)) (filter Lobby.playable (Foldable.toList (decks screen)))
      deck name = (name, Element.button (["deck"] <> ["picked" | picked screen == Just name]) (MkPick name) [Element.el "span" ["deck-name"] [] [Element.text (Lobby.label name)]])
      place p =
        Element.el
          "li"
          ["place"]
          []
          ( [Element.el "span" ["slot-title"] [] [Element.text (Link.who p)], Element.el "span" ["slot-deck"] [] [Element.text (Link.deck p)]]
              <> [Element.el "span" ["slot-ready"] [] [Element.string (if on then "Ready" else "Not ready")] | on <- Maybe.maybeToList (Link.ready p)]
          )
      readyButton =
        Element.el
          "button"
          (["primary", "start", "ready"] <> ["is-ready" | isReady screen])
          ([Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if isReady screen then "true" else "false")), Element.on "click" MkReady] <> [Element.attr "disabled" Text.empty | not (connected screen) || Maybe.isNothing (picked screen) || null (places screen)])
          [Element.string (if isReady screen then "Ready \x2713 \x00b7 waiting for every player" else "Ready")]
   in Element.keyed
        "main"
        ["lobby", "join"]
        []
        $ [ (Text.pack "top", Element.el "div" ["lobby-top"] [] [Element.el "h1" [] [] [Element.string "Join a game"]]),
            ( Text.pack "status",
              Element.el "section" ["join-status"] [Element.attr "aria-label" (Text.pack "Connection")] $
                if connected screen
                  then [Element.el "p" [] [] [Element.string "Connected to the host."], Element.el "ul" ["places"] [] (fmap place (places screen)), readyButton]
                  else case reply screen of
                    Nothing ->
                      [ Element.el "p" [] [] [Element.string "Choose your deck and make the code to send back to the host. A game under way picks up where it stopped, whatever deck you choose."],
                        Element.el "button" ["primary", "make-reply"] [Element.attr "type" (Text.pack "button"), Element.on "click" MkMakeReply] [Element.string "Make my reply code"]
                      ]
                    Just code ->
                      [ Element.el "label" [] [Element.attr "for" (Text.pack "join-reply")] [Element.string "Send this code back to the host, who pastes it in:"],
                        Element.el "input" ["invite-link"] [Element.attr "id" (Text.pack "join-reply"), Element.attr "type" (Text.pack "text"), Element.attr "readonly" Text.empty, Element.attr "value" code] [],
                        Element.el "p" [] [] [Element.string "Waiting for the host to connect\x2026"]
                      ]
            )
          ]
          <> [(Text.pack "problem", Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p]) | p <- Maybe.maybeToList (problem screen)]
          <> [ ( Text.pack "picker",
                 Element.el
                   "section"
                   ["deck-picker"]
                   []
                   [ Element.el
                       "div"
                       ["deck-tools"]
                       []
                       [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
                         Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] []
                       ],
                     Element.el "p" ["choosing-for"] [] [Element.string (maybe "Choose your deck." (\name -> "Your deck: " <> Text.unpack (Lobby.label name) <> ".") (picked screen))],
                     if null shown
                       then Element.el "p" ["empty"] [] [Element.string (if reading screen then "Reading the decks\x2026" else "No deck matches.")]
                       else Element.keyed "div" ["deck-list"] [] (fmap deck shown)
                   ]
               )
             ]
