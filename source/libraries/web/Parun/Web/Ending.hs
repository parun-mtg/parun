-- | The web client's last screen: how the game ended, a link saving it as a
-- scenario, one starting another, and everything the player saw happen.
module Parun.Web.Ending where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Uri.Fragment as Fragment
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html

data Ending = MkEnding
  { -- | The result and how each player left (Render.result,
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
  Html.element (Text.pack "main") [] $
    [Html.element (Text.pack "h1") [] [Html.text $ Text.pack "Game over"]]
      <> [Html.element (Text.pack "p") [] [Html.text line] | line <- said ending]
      <> [ Html.element
             (Text.pack "p")
             []
             [ Html.element (Text.pack "a") [property "href" $ download (scenario ending), property "download" $ Text.pack "parun-game.json"] [Html.text $ Text.pack "Save the game as a scenario"],
               Html.text $ Text.pack " ",
               -- The empty reference is the page itself, loaded afresh.
               Html.element (Text.pack "a") [property "href" Text.empty] [Html.text $ Text.pack "Play again"]
             ],
           Html.element (Text.pack "h2") [] [Html.text $ Text.pack "Game log"],
           Html.element (Text.pack "ol") [] [Html.element (Text.pack "li") [] [Html.text line] | line <- Foldable.toList $ witnessed ending]
         ]
  where
    property = Attribute.MkProperty . Text.pack

-- | A data URL holding the text as JSON (RFC 2397).
download :: Text.Text -> Text.Text
download contents =
  Text.pack "data:application/json;charset=utf-8,"
    <> Encoding.decodeLatin1 (LazyByteString.toStrict . Builder.toLazyByteString . Fragment.encode $ Encoding.encodeUtf8Builder contents)
