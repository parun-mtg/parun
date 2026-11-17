-- | The web client's first screen: how much of the corpus loaded, and a
-- button for each deck, tapped for the player's and then for the bot's.
module Parun.Web.Lobby where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Line as Line

data Lobby = MkLobby
  { -- | The card files the registry holds.
    cardFiles :: Natural,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | The player's deck, once tapped.
    first :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

view :: Lobby -> Html.Html Line.Line
view lobby =
  Html.element
    (Text.pack "main")
    []
    [ Html.element (Text.pack "h1") [] [Html.text $ Text.pack "Parun"],
      Html.element (Text.pack "p") [] [Html.text . Text.pack $ show (cardFiles lobby) <> " card files loaded."],
      Html.element (Text.pack "p") [] [Html.text $ maybe (Text.pack "Choose your deck.") (\deck -> Text.pack "You play " <> deck <> Text.pack ". Choose the bot's deck.") (first lobby)],
      Html.keyed (Text.pack "ul") [] . fmap (\name -> (name, Html.element (Text.pack "li") [] [Html.element (Text.pack "button") [Attribute.MkOn (Text.pack "click") (Line.MkFixed name)] [Html.text name]])) . Foldable.toList $ decks lobby
    ]
