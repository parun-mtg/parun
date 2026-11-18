-- | The web client's last screen: how the game ended for the player, a link
-- saving it as a scenario, one starting another, and everything the player
-- saw happen.
module Parun.Web.Ending where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Uri.Fragment as Fragment
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log

-- | How the game ended for the player the page plays.
data Verdict
  = MkWon
  | MkLost
  | MkDrawn
  | -- | Play stopped before the game ended.
    MkStopped
  deriving (Eq, Show)

data Ending = MkEnding
  { verdict :: Verdict,
    -- | The result and how each player left (Render.result,
    -- Render.departures), or why play stopped first.
    said :: [Text.Text],
    -- | The game as a scenario file (Command.rendered).
    scenario :: Text.Text,
    -- | What the player saw happen, oldest first (Log.unseen).
    witnessed :: Seq.Seq Log.Entry,
    -- | The seat the page plays.
    viewer :: PlayerId.PlayerId,
    -- | The player's deck and the bot's, by name (Lobby.deckName).
    decks :: (Text.Text, Text.Text)
  }
  deriving (Eq, Show)

view :: Ending -> Html.Html msg
view ending =
  let (headline, tone) = case verdict ending of
        MkWon -> ("You win!", "won")
        MkLost -> ("You lose", "lost")
        MkDrawn -> ("A draw", "drawn")
        MkStopped -> ("Game over", "stopped")
   in Element.el
        "main"
        ["ending"]
        []
        [ Element.el "section" ["verdict", tone] [] $
            Element.el "h1" [] [] [Element.string headline] : [Element.el "p" [] [] [Element.text line] | line <- said ending],
          Element.el
            "div"
            ["ending-actions"]
            []
            [ Element.el "a" ["primary"] [Element.attr "href" (rematch (decks ending))] [Element.string "Rematch"],
              -- The page itself without a query: the lobby.
              Element.el "a" [] [Element.attr "href" (Text.pack "./")] [Element.string "New game"],
              Element.el "a" [] [Element.attr "href" (download (scenario ending)), Element.attr "download" (Text.pack "parun-game.json")] [Element.string "Save as a scenario"]
            ],
          Element.el "h2" [] [] [Element.string "Game log"],
          Element.el "ol" ["game-log"] [] (fmap (Log.item (viewer ending) Nothing False) (Foldable.toList (witnessed ending)))
        ]

-- | The page starting a game between these decks at once (engine.js passes
-- the query on as parun_mine and parun_theirs). A deck's name is a path of
-- lowercase words, so nothing in it needs escaping in a query but what a
-- fragment escapes too.
rematch :: (Text.Text, Text.Text) -> Text.Text
rematch (mine, theirs) = Text.pack "?mine=" <> query mine <> Text.pack "&theirs=" <> query theirs
  where
    query t = Encoding.decodeLatin1 (LazyByteString.toStrict . Builder.toLazyByteString . Fragment.encode $ Encoding.encodeUtf8Builder t)

-- | A data URL holding the text as JSON (RFC 2397).
download :: Text.Text -> Text.Text
download contents =
  Text.pack "data:application/json;charset=utf-8,"
    <> Encoding.decodeLatin1 (LazyByteString.toStrict . Builder.toLazyByteString . Fragment.encode $ Encoding.encodeUtf8Builder contents)
