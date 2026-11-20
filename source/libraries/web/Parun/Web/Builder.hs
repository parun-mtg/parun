-- | The deck builder: a deck made from a card pool, searched as it is
-- typed, checked as it changes, and saved as a deck list.
module Parun.Web.Builder where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Web.CardIndex as CardIndex
import qualified Parun.Web.Lobby as Lobby
import qualified Text.Read as Read

data Builder = MkBuilder
  { -- | Every card the deck may draw on, once indexed.
    index :: Maybe (Seq.Seq CardIndex.Entry),
    query :: CardIndex.Query,
    -- | The main deck: how many of each card.
    deck :: Map.Map CardName.CardName Natural,
    -- | What follows a main-deck card's name on its line: its printing and
    -- annotations, from a list opened.
    suffixes :: Map.Map CardName.CardName Text.Text,
    -- | Every line of a list opened after its main deck, kept as written.
    rest :: [Text.Text],
    -- | The format the list's header names.
    format :: Text.Text,
    -- | Every shipped format, by name.
    formats :: [Text.Text],
    -- | The fewest cards the format allows, once known.
    least :: Maybe Natural,
    -- | Why the list as it stands breaks its format.
    problems :: [Text.Text],
    -- | The cards those problems are about.
    flagged :: Set.Set CardName.CardName,
    -- | The card whose text is shown.
    reading :: Maybe CardName.CardName,
    -- | The name typed to save under.
    saveName :: Text.Text,
    -- | The saved deck this one is, once saved.
    savedAs :: Maybe Text.Text,
    -- | Why the last save was refused.
    saveProblems :: [Text.Text],
    -- | Whether the deck changed since it was last saved.
    dirty :: Bool,
    -- | Whether leaving with changes is being asked about.
    leaving :: Bool
  }
  deriving (Eq, Show)

-- | What a tap or a keystroke in the builder does.
data Event
  = MkSearch
  | MkToggleColour Color.Color
  | MkToggleKind CardType.CardType
  | MkValue
  | MkAdd CardName.CardName
  | MkRemove CardName.CardName
  | MkRead CardName.CardName
  | MkCloseCard
  | MkFormat
  | MkSaveName
  | MkSave
  | MkBack
  | MkStay
  | MkLeave
  deriving (Eq, Show)

-- | An empty deck under constructed, among these formats, its cards not
-- yet indexed.
new :: [Text.Text] -> Builder
new named = MkBuilder Nothing CardIndex.everything Map.empty Map.empty [] (Text.pack "constructed") named Nothing [] Set.empty Nothing Text.empty Nothing [] False False

-- | The builder after an event, the value typed being `typed`. Save, Leave,
-- and Back with nothing changed are the shell's to carry out.
update :: Event -> Text.Text -> Builder -> Builder
update event typed builder = case event of
  MkSearch -> withQuery (\q -> q {CardIndex.text = typed})
  MkToggleColour c -> withQuery (\q -> q {CardIndex.wanted = toggle c (CardIndex.wanted q)})
  MkToggleKind k -> withQuery (\q -> q {CardIndex.kinds = toggle k (CardIndex.kinds q)})
  MkValue -> withQuery (\q -> q {CardIndex.valued = Read.readMaybe (Text.unpack typed)})
  MkAdd card -> changed builder {deck = Map.insertWith (+) card 1 (deck builder)}
  MkRemove card
    | Map.member card (deck builder) -> changed builder {deck = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) card (deck builder)}
    | otherwise -> builder
  MkRead card -> builder {reading = Just card}
  MkCloseCard -> builder {reading = Nothing}
  MkFormat -> changed builder {format = typed}
  MkSaveName -> builder {saveName = typed}
  MkSave -> builder
  MkBack -> builder {leaving = dirty builder}
  MkStay -> builder {leaving = False}
  MkLeave -> builder
  where
    withQuery f = builder {query = f (query builder)}
    changed b = b {dirty = True}
    toggle :: (Ord a) => a -> Set.Set a -> Set.Set a
    toggle x xs = if Set.member x xs then Set.delete x xs else Set.insert x xs

-- | The deck as a deck list: its format's header, the main deck by name,
-- then the rest of a list opened, as written.
listText :: Builder -> Text.Text
listText builder =
  Text.unlines $
    (Text.pack "Format: " <> format builder)
      : [Text.pack (show n) <> Text.singleton ' ' <> CardName.unwrap card <> Map.findWithDefault Text.empty card (suffixes builder) | (card, n) <- List.sortOn (CardName.unwrap . fst) (Map.toList (deck builder))]
        <> rest builder

-- | The builder with a check's problems, and the cards they name.
checked :: [Text.Text] -> Set.Set CardName.CardName -> Builder -> Builder
checked found named builder = builder {problems = found, flagged = named}

-- | The builder once saved as this deck.
saved :: Text.Text -> Builder -> Builder
saved name builder = builder {savedAs = Just name, dirty = False, saveProblems = []}

-- | The saved deck a save writes, given the saved decks' names: the one
-- this deck was saved as, or else the one the typed name makes, unless it
-- may not be used (Lobby.nameProblems).
saveTarget :: Seq.Seq Text.Text -> Builder -> Either [Text.Text] Text.Text
saveTarget decks builder = case savedAs builder of
  Just name -> Right name
  Nothing -> case Lobby.nameProblems decks (saveName builder) of
    [] -> Right (Lobby.savedPrefix <> Lobby.slugOf (saveName builder))
    problems' -> Left problems'
