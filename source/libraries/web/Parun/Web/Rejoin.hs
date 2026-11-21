-- | The screen a game with a guest shows once their connection drops
-- (#9187), on either page: two codes swapped by hand again, as when the
-- guest first joined (Parun.Web.Join), connect the pages anew, and the game
-- goes on from the request pending (Parun.Web.Link.remote).
module Parun.Web.Rejoin where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

data Rejoin = MkRejoin
  { -- | Which page shows it.
    side :: Side,
    -- | What is pasted: the guest's reply code on the host's page, the
    -- host's new link on the guest's.
    typed :: Text.Text,
    -- | The code to send the other page, once made: the host's link, or the
    -- guest's reply.
    code :: Maybe Text.Text,
    -- | Why the last try did not connect.
    problem :: Maybe Text.Text
  }
  deriving (Eq, Show)

data Side
  = -- | The host's page, waiting on the guest playing this seat ("Player 2").
    MkHost Text.Text
  | MkGuest
  deriving (Eq, Show)

data Event
  = -- | The text pasted.
    MkTyped
  | -- | Connects with the reply code pasted (the host's page), or makes the
    -- reply code from the link pasted (the guest's); the shell does it.
    MkConnect
  | -- | Gives the game up: the host's page stops it, the guest's goes home.
    MkGiveUp
  deriving (Eq, Show)

-- | The screen as the connection to the guest playing this seat drops.
host :: Text.Text -> Rejoin
host who = MkRejoin (MkHost who) Text.empty Nothing Nothing

-- | The screen as the guest's connection to the host drops.
guest :: Rejoin
guest = MkRejoin MkGuest Text.empty Nothing Nothing

-- | The screen after an event, the value typed being `value`.
update :: Event -> Text.Text -> Rejoin -> Rejoin
update event value screen = case event of
  MkTyped -> screen {typed = value}
  _ -> screen

-- | The host's code in what the guest pasted: the link's #offer= part, or
-- the code itself.
offerIn :: Text.Text -> Text.Text
offerIn pasted =
  let (before, after) = Text.breakOnEnd (Text.pack "offer=") (Text.strip pasted)
   in Text.strip (if Text.null before then after else Text.takeWhile (/= '&') after)

view :: Rejoin -> Html.Html Event
view screen =
  Element.el "main" ["lobby", "rejoin"] [] $
    [ Element.el "h1" [] [] [Element.string (case side screen of MkHost who -> Text.unpack who <> "\x2019s connection dropped"; MkGuest -> "The connection to the host dropped")],
      Element.keyed "section" ["join-status"] [Element.attr "aria-label" (Text.pack "Connection")] steps
    ]
      <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (problem screen)]
      <> [Element.button ["give-up"] MkGiveUp [Element.string (case side screen of MkHost _ -> "Give the game up"; MkGuest -> "Home")]]
  where
    -- Each step's boxes are keyed apart, and the host's reply box by its
    -- link: a box kept across steps would keep the text pasted into it.
    box name = [Element.attr "id" (Text.pack name), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "spellcheck" (Text.pack "false"), Element.on "input" MkTyped]
    shown name value = Element.el "input" ["invite-link"] [Element.attr "id" (Text.pack name), Element.attr "type" (Text.pack "text"), Element.attr "readonly" Text.empty, Element.attr "value" value] []
    connect label = Element.el "button" ["primary", "connect"] ([Element.attr "type" (Text.pack "button"), Element.on "click" MkConnect] <> [Element.attr "disabled" Text.empty | Text.null (Text.strip (typed screen))]) [Element.string label]
    steps = case (side screen, code screen) of
      (MkHost _, Nothing) -> [(Text.pack "making", Element.el "p" ["invite-step"] [] [Element.string "Making a new link\x2026"])]
      (MkHost _, Just url) ->
        [ (Text.pack "waits", Element.el "p" [] [] [Element.string "The game waits for them. To pick it up where it stopped:"]),
          (Text.pack "link-label", Element.el "label" [] [Element.attr "for" (Text.pack "rejoin-link")] [Element.string "1. Send them this new link"]),
          (Text.pack "link", shown "rejoin-link" url),
          (Text.pack "reply-label", Element.el "label" [] [Element.attr "for" (Text.pack "rejoin-reply")] [Element.string "2. Paste the code they send back"]),
          (Text.pack "reply " <> url, Element.el "input" ["invite-reply"] (box "rejoin-reply") []),
          (Text.pack "connect", connect "Connect")
        ]
      (MkGuest, Nothing) ->
        [ (Text.pack "offer-label", Element.el "label" [] [Element.attr "for" (Text.pack "rejoin-offer")] [Element.string "The game waits for you. Paste the new link the host sends you:"]),
          (Text.pack "offer", Element.el "input" ["invite-reply"] (box "rejoin-offer") []),
          (Text.pack "make", connect "Make my reply code")
        ]
      (MkGuest, Just reply) ->
        [ (Text.pack "code-label", Element.el "label" [] [Element.attr "for" (Text.pack "rejoin-code")] [Element.string "Send this code back to the host, who pastes it in:"]),
          (Text.pack "code", shown "rejoin-code" reply),
          (Text.pack "waiting", Element.el "p" [] [] [Element.string "Waiting for the host to connect\x2026"])
        ]
