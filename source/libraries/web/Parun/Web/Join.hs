-- | The screen a link from a host opens (#9185): the guest picks their own
-- deck in the browser the lobby shares (Parun.Web.DeckBrowser), sends back
-- the reply code it makes, and once connected sees the whole match-up and
-- the game's options and says when they are ready. A link to a game under
-- way, or to one whose decks are fixed (Route.MkSeated), skips the deck.
module Parun.Web.Join where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Route as Route

data Join = MkJoin
  { -- | The decks, what is read of them, and what they are filtered by, as
    -- the lobby's are (Parun.Web.Lobby.browser).
    browser :: DeckBrowser.DeckBrowser,
    -- | The deck chosen.
    picked :: Maybe Text.Text,
    -- | The code to send the host, once made.
    reply :: Maybe Text.Text,
    -- | Whether the host's page is connected.
    connected :: Bool,
    -- | The match-up as the host last sent it.
    places :: [Link.Place],
    -- | The game's options as the host last sent them (Lobby.told), each a
    -- label and its value; none for a game under way.
    options :: [(Text.Text, Text.Text)],
    -- | Why the connection failed or ended.
    problem :: Maybe Text.Text,
    -- | Whether the game joined has started, as the host's link says.
    stage :: Route.Stage
  }
  deriving (Eq, Show)

data Event
  = -- | Makes this deck yours.
    MkPick Text.Text
  | -- | Something done in the deck browser.
    MkBrowse DeckBrowser.Event
  | -- | Makes the reply code; the shell does it.
    MkMakeReply
  | -- | Toggles whether you are ready; the shell tells the host.
    MkReady
  deriving (Eq, Show)

-- | A join screen for a game at this stage, its decks still to read.
new :: Route.Stage -> Join
new = MkJoin DeckBrowser.new Nothing Nothing False [] [] Nothing

-- | Whether the guest brings no deck: the game is under way, or its decks
-- are fixed.
decksSet :: Join -> Bool
decksSet screen = stage screen /= Route.MkGathering

-- | Whether the reply code may be made: once a deck is chosen, or at once
-- where the guest brings none ('decksSet').
canReply :: Join -> Bool
canReply screen = decksSet screen || Maybe.isJust (picked screen)

-- | The screen after an event, the value typed being `typed`.
update :: Event -> Text.Text -> Join -> Join
update event typed screen = case event of
  MkPick name | Foldable.elem name (DeckBrowser.decks (browser screen)) -> screen {picked = Just name}
  MkBrowse browsing -> screen {browser = DeckBrowser.update browsing typed (browser screen)}
  _ -> screen

-- | The decks a guest may play: every one to play (Lobby.playable).
offered :: Join -> Seq.Seq Text.Text
offered = Seq.filter Lobby.playable . DeckBrowser.decks . browser

-- | Your deck as the host holds it: the one chosen here, else the one the
-- host last said you play, as after a Rematch of a game rejoined.
yourDeck :: Join -> Maybe Text.Text
yourDeck screen = case picked screen of
  Just name -> Just (snd (DeckBrowser.describe (DeckBrowser.titles (browser screen)) name))
  Nothing -> List.find (/= Link.unchosen) [Link.deck p | p <- places screen, Link.who p == Text.pack "You"]

-- | The screen a Rematch returns to, still connected: the match-up just
-- played, nobody ready, until the host tells it afresh, in the lobby.
rematched :: Join -> Join
rematched screen = screen {connected = True, problem = Nothing, places = fmap (\p -> p {Link.ready = False <$ Link.ready p}) (places screen), stage = Route.MkGathering}

-- | Whether the host last said you are ready.
isReady :: Join -> Bool
isReady screen = Maybe.fromMaybe False (List.find ((== Text.pack "You") . Link.who) (places screen) >>= Link.ready)

view :: Join -> Html.Html Event
view screen =
  let place p =
        Element.el
          "li"
          ["place"]
          []
          ( [Element.el "span" ["slot-title"] [] [Element.text (Link.who p)]]
              <> [Element.el "span" ["chip", "bot"] [] [Element.string "Bot"] | Link.bot p]
              <> [Element.el "span" ["slot-deck"] [] [Element.text (Link.deck p)]]
              <> [Element.el "span" ["slot-ready"] [] [Element.string (if on then "Ready" else "Not ready")] | on <- Maybe.maybeToList (Link.ready p)]
          )
      option (label, value) = Element.el "li" ["place"] [] [Element.el "span" ["slot-title"] [] [Element.text label], Element.el "span" ["slot-deck"] [] [Element.text value]]
      optionsList = [Element.el "ul" ["places", "told-options"] [Element.attr "aria-label" (Text.pack "Game options")] (fmap option (options screen)) | not (null (options screen))]
      readyButton =
        Element.el
          "button"
          (["primary", "start", "ready"] <> ["is-ready" | isReady screen])
          ([Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if isReady screen then "true" else "false")), Element.on "click" MkReady] <> [Element.attr "disabled" Text.empty | not (connected screen) || Maybe.isNothing (yourDeck screen) || null (places screen)])
          [Element.string (if isReady screen then "Ready \x2713 \x00b7 waiting for every player" else "Ready")]
   in Element.keyed
        "main"
        ["lobby", "join"]
        []
        $ [ (Text.pack "top", Element.el "div" ["lobby-top"] [] [Element.el "h1" [] [] [Element.string "Join a game"]]),
            ( Text.pack "status",
              Element.el "section" ["join-status"] [Element.attr "aria-label" (Text.pack "Connection")] $
                if connected screen
                  then [Element.el "p" [] [] [Element.string "Connected to the host."], Element.el "ul" ["places"] [] (fmap place (places screen))] <> optionsList <> [readyButton]
                  else case reply screen of
                    Nothing ->
                      [ Element.el "p" [] [] [Element.string (prompt (stage screen))],
                        Element.el "button" ["primary", "make-reply"] ([Element.attr "type" (Text.pack "button"), Element.on "click" MkMakeReply] <> [Element.attr "disabled" Text.empty | not (canReply screen)]) [Element.string "Make my reply code"]
                      ]
                    Just code ->
                      [ Element.el "label" [] [Element.attr "for" (Text.pack "join-reply")] [Element.string "Send this code back to the host, who pastes it in:"],
                        Element.copyable "join-reply" code,
                        Element.el "p" [] [] [Element.string "Waiting for the host to connect\x2026"]
                      ]
            )
          ]
          <> [(Text.pack "problem", Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p]) | p <- Maybe.maybeToList (problem screen)]
          <> picker
  where
    browsing = browser screen
    prompt stage' = case stage' of
      Route.MkGathering -> "Choose your deck and make the code to send back to the host."
      Route.MkSeated -> "This game plays on from a replay's position, each seat's deck set: make the code to send back to the host."
      Route.MkUnderWay -> "This game is under way: make the code to send back to the host, and it picks up where it stopped."
    picker
      | decksSet screen = []
      | otherwise =
          [ ( Text.pack "picker",
              Element.el
                "section"
                ["browser"]
                [Element.attr "aria-label" (Text.pack "Decks")]
                [ Element.el "h2" [] [] [Element.string (maybe "Choose your deck" (\name -> "Your deck: " <> Text.unpack name) (yourDeck screen))],
                  fmap MkBrowse (DeckBrowser.shelvesView browsing),
                  DeckBrowser.filtersView MkBrowse [] browsing,
                  DeckBrowser.resultsView MkPick ((== picked screen) . Just) [Element.el "p" ["empty"] [] [Element.string "No saved decks in this browser."]] (offered screen) browsing
                ]
            ),
            ( Text.pack "detail",
              Element.el "aside" ["detail"] [Element.attr "aria-label" (Text.pack "Deck chosen")] $
                Element.el "span" ["eyebrow"] [] [Element.string "Your deck"]
                  : maybe [Element.el "p" ["muted"] [] [Element.string "Pick one of the decks to see it here."]] (DeckBrowser.detailBody browsing) (picked screen)
            )
          ]
