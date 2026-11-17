-- | The web client's first screen: how much of the corpus loaded, and the
-- decks a game can be between.
module Parun.Web.Lobby where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Web.Html as Html

data Lobby = MkLobby
  { -- | The card files the registry holds.
    cardFiles :: Natural,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text
  }
  deriving (Eq, Show)

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

view :: Lobby -> Html.Html msg
view lobby =
  Html.element
    (Text.pack "main")
    []
    [ Html.element (Text.pack "h1") [] [Html.text $ Text.pack "Parun"],
      Html.element (Text.pack "p") [] [Html.text . Text.pack $ show (cardFiles lobby) <> " card files loaded."],
      Html.keyed (Text.pack "ul") [] . fmap (\name -> (name, Html.element (Text.pack "li") [] [Html.text name])) . Foldable.toList $ decks lobby
    ]
