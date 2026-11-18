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
import qualified Parun.Uri.Fragment as Fragment
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html

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
    -- | What the player saw happen, oldest first (Client.unseen).
    witnessed :: Seq.Seq Text.Text
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
            [ -- The empty reference is the page itself, loaded afresh.
              Element.el "a" ["primary"] [Element.attr "href" Text.empty] [Element.string "Play again"],
              Element.el "a" [] [Element.attr "href" (download (scenario ending)), Element.attr "download" (Text.pack "parun-game.json")] [Element.string "Save as a scenario"]
            ],
          Element.el "h2" [] [] [Element.string "Game log"],
          Element.el "ol" ["game-log"] [] [Element.el "li" [] [] [Element.text line] | line <- Foldable.toList (witnessed ending)]
        ]

-- | A data URL holding the text as JSON (RFC 2397).
download :: Text.Text -> Text.Text
download contents =
  Text.pack "data:application/json;charset=utf-8,"
    <> Encoding.decodeLatin1 (LazyByteString.toStrict . Builder.toLazyByteString . Fragment.encode $ Encoding.encodeUtf8Builder contents)
