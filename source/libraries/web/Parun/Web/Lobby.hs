-- | The web client's lobby (#9421): the seats at the table, the seat being
-- chosen for, which only a tap on a seat changes, and Start at their foot;
-- the decks that seat may play, filtered by what is typed, by colour, by set
-- and by whether they are saved; and the chosen deck's detail.
module Parun.Web.Lobby where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.RuleFamily as RuleFamily.Engine
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Mana as Mana
import qualified Text.Read as Read

data Lobby = MkLobby
  { -- | Whether the deck lists are still to read.
    reading :: Bool,
    -- | Why the deck lists did not load.
    problem :: Maybe Text.Text,
    -- | Every deck, by its path beneath the deck root, less ".txt".
    decks :: Seq.Seq Text.Text,
    -- | The format each deck's header names (Parun.DeckList.formatOf), by
    -- name; a deck naming none is absent.
    formats :: Map.Map Text.Text Text.Text,
    -- | The player's deck, once chosen.
    mine :: Maybe Text.Text,
    -- | Each bot's deck, in seat order, once chosen; a random one otherwise.
    -- There is at least one bot and at most 'mostBots'.
    theirs :: [Maybe Text.Text],
    -- | The seat a tapped deck goes to, which only a tap on a seat changes.
    choosing :: Slot,
    -- | What the deck list is filtered by.
    search :: Text.Text,
    -- | The colours a deck shown must hold, each of them.
    colourFilter :: Set.Set Color.Color,
    -- | The one set whose decks are shown, by its code (describe); every
    -- set's while absent.
    setFilter :: Maybe Text.Text,
    -- | Whether the shipped decks are shown or the saved ones.
    shelf :: Shelf,
    -- | Each deck's colours, by its name (colours).
    palette :: Map.Map Text.Text (Set.Set Color.Color),
    -- | The card lists of the decks chosen so far, by name.
    previews :: Map.Map Text.Text [Line],
    -- | The panel a deck list is pasted into, while open.
    importing :: Maybe Import,
    -- | A saved deck's name and text, shown to copy.
    exporting :: Maybe (Text.Text, Text.Text),
    -- | Why each saved deck found to break its format does, by name.
    illegal :: Map.Map Text.Text [Text.Text],
    -- | How many cards each deck's main deck holds, by name, as read so far.
    sizes :: Map.Map Text.Text Natural,
    -- | Each deck's spells by mana value (curveOf), by name, as read so far.
    curves :: Map.Map Text.Text [Natural],
    -- | Each bot's slot switched to a player invited from another browser,
    -- by its place in 'theirs' (Parun.Web.Link).
    invites :: Map.Map Int Invite,
    -- | Whether you are ready to start a game with the players invited.
    ready :: Bool
  }
  deriving (Eq, Show)

-- | A slot a player from another browser takes.
data Invite = MkInvite
  { -- | The link to send them, once made.
    link :: Maybe Text.Text,
    -- | The reply code they sent back, as typed.
    replyCode :: Text.Text,
    -- | Whether their page is connected.
    connected :: Bool,
    -- | Their deck's name and list, once they chose one.
    guestDeck :: Maybe (Text.Text, Text.Text),
    -- | Whether they are ready to start.
    guestReady :: Bool,
    -- | Why the connection failed or ended.
    inviteProblem :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | An invite just made: no link yet, nothing typed.
newInvite :: Invite
newInvite = MkInvite Nothing Text.empty False Nothing False Nothing

-- | A deck list being pasted in, to save under a name.
data Import = MkImport
  { importName :: Text.Text,
    importText :: Text.Text,
    -- | Why the last import was refused.
    importProblems :: [Text.Text],
    -- | Whether the list is being checked.
    checking :: Bool
  }
  deriving (Eq, Show)

-- | Which decks the browser shows.
data Shelf
  = -- | The decks Parun ships, each a set's.
    MkPrecons
  | -- | The decks saved in this browser.
    MkSaved
  deriving (Eq, Show)

-- | A side of the match-up.
data Slot
  = MkMine
  | -- | The bot at this place in 'theirs', from zero.
    MkTheirs Int
  deriving (Eq, Show)

-- | What a tap or a keystroke on the lobby does.
data Event
  = -- | Makes this deck the chosen slot's.
    MkPick Text.Text
  | -- | Chooses a deck for this slot next.
    MkChoose Slot
  | -- | Filters the decks by the text typed.
    MkSearch
  | -- | Shows only the decks holding this colour, or stops.
    MkColour Color.Color
  | -- | Shows only the set whose code is chosen, or every set for none.
    MkSet
  | -- | Shows these decks.
    MkShelf Shelf
  | -- | A random deck of those shown, for the slot being chosen; the shell
    -- picks it.
    MkRandom
  | -- | Seats another bot, with a random deck, up to 'mostBots'.
    MkAddBot
  | -- | Unseats the last bot, while there are two or more.
    MkDropBot
  | -- | Starts the game.
    MkStart
  | -- | Opens the panel a deck list is pasted into.
    MkOpenImport
  | -- | Closes it.
    MkCloseImport
  | -- | The name typed for the deck pasted.
    MkImportName
  | -- | The deck list typed or pasted.
    MkImportText
  | -- | Checks the list and saves it; the shell does both.
    MkSubmitImport
  | -- | Deletes this saved deck; the shell does it.
    MkDelete Text.Text
  | -- | Shows this saved deck's list to copy; the shell reads it.
    MkExport Text.Text
  | -- | Hides the list shown to copy.
    MkCloseExport
  | -- | Opens the deck builder; the shell does it.
    MkOpenBuilder
  | -- | Opens this deck in the builder, a saved one to save over, any other
    -- as a copy; the shell does it.
    MkEdit Text.Text
  | -- | Switches this bot's slot to a player invited from another browser,
    -- or seats one past the last bot; the shell makes the link.
    MkInviteSlot Int
  | -- | Switches this invited slot back to a bot; the shell closes the link.
    MkBotSlot Int
  | -- | The reply code typed for this invited slot.
    MkReplyCode Int
  | -- | Connects this invited slot with the reply code typed; the shell
    -- does it.
    MkConnect Int
  | -- | Toggles whether you are ready to start with the players invited.
    MkReady
  deriving (Eq, Show)

-- | A lobby whose decks are still to read, nothing chosen yet.
new :: Lobby
new =
  MkLobby
    { reading = True,
      problem = Nothing,
      decks = Seq.empty,
      formats = Map.empty,
      mine = Nothing,
      theirs = [Nothing],
      choosing = MkMine,
      search = Text.empty,
      colourFilter = Set.empty,
      setFilter = Nothing,
      shelf = MkPrecons,
      palette = Map.empty,
      previews = Map.empty,
      importing = Nothing,
      exporting = Nothing,
      illegal = Map.empty,
      sizes = Map.empty,
      curves = Map.empty,
      invites = Map.empty,
      ready = False
    }

-- | Where saved decks sit beneath the deck root.
savedPrefix :: Text.Text
savedPrefix = Text.pack "saved/"

-- | Whether a deck is one to play: not a set's whole card list.
playable :: Text.Text -> Bool
playable name = isSaved name || not (Text.isSuffixOf (Text.pack "/all") name)

-- | Whether a deck is one the player saved.
isSaved :: Text.Text -> Bool
isSaved = Text.isPrefixOf savedPrefix

-- | A deck's name as its file's: lowercase letters and digits, each run of
-- anything else one dash, none at either end.
slugOf :: Text.Text -> Text.Text
slugOf = Text.intercalate (Text.pack "-") . filter (not . Text.null) . Text.split (not . Char.isAlphaNum) . Text.toLower

-- | Why a deck may not be saved under this name, among these decks: an
-- empty one, or one a saved deck holds.
nameProblems :: Seq.Seq Text.Text -> Text.Text -> [Text.Text]
nameProblems names typed
  | Text.null slug = [Text.pack "Give the deck a name."]
  | Foldable.elem (savedPrefix <> slug) names = [Text.pack "A saved deck is already named that."]
  | otherwise = []
  where
    slug = slugOf typed

-- | The lobby once this deck is imported: the panel closed, the saved decks
-- shown, and the deck the seat's being chosen for, or, where that bot may
-- not play it, yours, your seat then chosen so the chip shows where it went.
imported :: Text.Text -> Lobby -> Lobby
imported name lobby =
  let picked = update (MkPick name) Text.empty lobby
      seated = if chosen picked (choosing picked) == Just name then picked else update (MkPick name) Text.empty lobby {choosing = MkMine}
   in seated {importing = Nothing, shelf = MkSaved}

-- | The lobby without this deck, every seat holding it emptied.
deleted :: Text.Text -> Lobby -> Lobby
deleted name lobby =
  let emptied :: Maybe Text.Text -> Maybe Text.Text
      emptied = Monad.mfilter (/= name)
   in lobby
        { decks = Seq.filter (/= name) (decks lobby),
          formats = Map.delete name (formats lobby),
          palette = Map.delete name (palette lobby),
          previews = Map.delete name (previews lobby),
          curves = Map.delete name (curves lobby),
          mine = emptied (mine lobby),
          theirs = fmap emptied (theirs lobby),
          exporting = Nothing
        }

-- | The lobby holding these decks under these formats, what was chosen and
-- typed kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Lobby -> Lobby
withDecks names named lobby = lobby {decks = names, formats = named, reading = False}

-- | The most bots a game seats: a table of four.
mostBots :: Int
mostBots = 3

-- | Whether two decks are under one format, and so may meet: the game's
-- format is the one their headers name (Parun.Play.Command.settle).
together :: Lobby -> Text.Text -> Text.Text -> Bool
together lobby one two = case (Map.lookup one (formats lobby), Map.lookup two (formats lobby)) of
  -- A Limited deck meets a deck naming no format that a Limited game takes,
  -- since the game is then Limited (Parun.Play.Command.gameFormat): the
  -- client's reading, so a sealed deck has opponents. A deck not yet
  -- counted is offered, and Start refuses it if it is too small.
  (Just named, Nothing) | named == limited -> bigEnough two
  (Nothing, Just named) | named == limited -> bigEnough one
  (mine', theirs') -> mine' == theirs'
  where
    limited = Text.pack "limited"
    bigEnough name = maybe True (>= RuleFamily.Engine.minimumDeckSize RuleFamily.Limited) (Map.lookup name (sizes lobby))

-- | The decks the bot may play against yours: every deck, until yours is
-- chosen.
opponents :: Lobby -> Seq.Seq Text.Text
opponents lobby = maybe (decks lobby) (\one -> Seq.filter (together lobby one) (decks lobby)) (mine lobby)

-- | A card's colours as a deck shows them: those of the mana symbols in
-- its faces' costs.
cardColours :: Card.Card -> Set.Set Color.Color
cardColours c = foldMap (foldMap costColours . Face.manaCost) (Card.faces c)

-- | The colours a cost's symbols name (CR 105.2, 202.2).
costColours :: ManaCost.ManaCost -> Set.Set Color.Color
costColours printed =
  let ofType t = case t of
        ManaType.Colored c -> Set.singleton c
        ManaType.Colorless -> Set.empty
      symbol sym = case sym of
        ManaSymbol.Generic _ -> Set.empty
        ManaSymbol.OfType t -> ofType t
        ManaSymbol.Hybrid h -> Set.union (ofType (Hybrid.left h)) (ofType (Hybrid.right h))
        ManaSymbol.MonocoloredHybrid t -> ofType t
        ManaSymbol.Phyrexian c -> Set.singleton c
        ManaSymbol.HybridPhyrexian h -> Set.fromList [HybridPhyrexian.left h, HybridPhyrexian.right h]
        ManaSymbol.Snow -> Set.empty
        ManaSymbol.Variable -> Set.empty
   in foldMap symbol (ManaCost.unwrap printed)

-- | A deck list's colours: those of each card a line names, by `colours`.
deckColours :: (CardName.CardName -> Set.Set Color.Color) -> Text.Text -> Set.Set Color.Color
deckColours colours contents = foldMap colours (Set.fromList (fmap snd (entries contents)))

-- | How many cards a deck list's main deck holds: its lines before any
-- other section's header, and those under the main deck's.
mainCount :: Text.Text -> Natural
mainCount = sum . fmap fst . mainEntries

-- | A deck list's main deck ('entries'): its lines before any other
-- section's header, and those under the main deck's.
mainEntries :: Text.Text -> [(Natural, CardName.CardName)]
mainEntries contents =
  let step (inMain, kept) line = case DeckList.sectionNamed (Text.strip line) of
        Just section -> (section == DeckList.Main, kept)
        Nothing
          | inMain -> (inMain, kept <> entries line)
          | otherwise -> (inMain, kept)
   in snd (List.foldl' step (True, []) (Text.lines contents))

-- | A deck list's lines that name a card ("4 Forest", "1 Shock (M19) 156"),
-- each as its count and the card; other lines skipped.
entries :: Text.Text -> [(Natural, CardName.CardName)]
entries contents =
  let entry line = do
        let (count, rest) = Text.span Char.isDigit (Text.strip line)
        n <- Read.readMaybe (Text.unpack count)
        pure (n, CardName.MkCardName (Text.strip (fst (Text.breakOn (Text.pack " (") rest))))
   in Maybe.mapMaybe entry (Text.lines contents)

-- | A card of a deck's list, as the lobby previews it.
data Line = MkLine
  { copies :: Natural,
    card :: CardName.CardName,
    cost :: Maybe ManaCost.ManaCost,
    -- | CR 202.3.
    value :: Integer,
    kind :: Kind
  }
  deriving (Eq, Show)

-- | Which part of a list a card is shown in.
data Kind
  = MkCreature
  | MkSpell
  | MkLand
  deriving (Eq, Ord, Show)

-- | A deck's spells by mana value, from zero to five, then six and up.
curveOf :: [Line] -> [Natural]
curveOf lines' =
  let spells = [l | l <- lines', kind l /= MkLand]
   in [sum [copies l | l <- spells, value l == n] | n <- [0 .. 5]] <> [sum [copies l | l <- spells, value l >= 6]]

-- | A curve as bars, each labelled with its mana value, with these classes:
-- "spark" draws it small, unlabelled.
curve :: [String] -> [Natural] -> Html.Html msg
curve classes counts =
  let tallest = maximum (1 : counts)
      bar (mv, n) =
        Element.el
          "span"
          ["bar"]
          [Element.attr "title" (Text.pack (show n <> " at mana value " <> mv))]
          [Element.el "span" ["fill"] [Element.attr "style" (Text.pack ("height:" <> show (div (n * 100) tallest) <> "%"))] [], Element.el "span" ["mv"] [] [Element.string mv]]
   in Element.el "span" ("curve" : classes) [Element.attr "aria-hidden" (Text.pack "true")] (fmap bar (zip (fmap show [0 :: Int .. 5] <> ["6+"]) counts))

-- | A deck's cards by kind, each kind under its count.
cardList :: [Line] -> Html.Html msg
cardList lines' =
  let section title k =
        let these = List.sortOn (\l -> (value l, CardName.unwrap (card l))) [l | l <- lines', kind l == k]
         in [ Element.el "div" ["deck-section"] [] $
                Element.el "h3" [] [] [Element.string (title <> " \x00b7 " <> show (sum (fmap copies these)))]
                  : [Element.el "p" ["deck-line"] [] ([Element.el "span" ["copies"] [] [Element.string (show (copies l))], Element.el "span" ["deck-card"] [] [Element.text (CardName.unwrap (card l))]] <> foldMap (pure . Mana.cost) (cost l)) | l <- these]
            | not (null these)
            ]
   in Element.el "div" ["deck-sections"] [] (foldMap (uncurry section) kinds)

-- | Each kind of card, as a deck's detail names it.
kinds :: [(String, Kind)]
kinds = [("Creatures", MkCreature), ("Other spells", MkSpell), ("Lands", MkLand)]

-- | A deck file's name beneath the root, less ".txt"; the path as given
-- where the root does not prefix it.
deckName :: FilePath -> FilePath -> Text.Text
deckName root path =
  let relative = Maybe.fromMaybe path $ List.stripPrefix (root <> "/") path
      name = Text.pack relative
   in Maybe.fromMaybe name $ Text.stripSuffix (Text.pack ".txt") name

-- | The lobby after an event, the value typed being `typed`. A pick goes to
-- the seat being chosen for and never moves it on; picking your deck drops
-- each bot's deck under another format, and a bot is offered only its
-- 'opponents'.
update :: Event -> Text.Text -> Lobby -> Lobby
update event typed lobby = settled lobby (updated event typed lobby)

-- | 'update' before any ready flag is cleared.
updated :: Event -> Text.Text -> Lobby -> Lobby
updated event typed lobby = case event of
  MkPick name
    | Foldable.notElem name (decks lobby) -> lobby
    | MkTheirs i <- choosing lobby ->
        if Foldable.notElem name (opponents lobby) then lobby else lobby {theirs = [if j == i then Just name else deck | (j, deck) <- zip [0 ..] (theirs lobby)]}
    | otherwise -> lobby {mine = Just name, theirs = fmap (Monad.mfilter (together lobby name)) (theirs lobby)}
  MkChoose slot
    | MkTheirs i <- slot, Map.member i (invites lobby) -> lobby
    | otherwise -> lobby {choosing = slot}
  MkSearch -> lobby {search = typed}
  MkColour c -> lobby {colourFilter = (if Set.member c (colourFilter lobby) then Set.delete c else Set.insert c) (colourFilter lobby)}
  MkSet -> lobby {setFilter = if Text.null typed then Nothing else Just typed}
  MkShelf which -> lobby {shelf = which}
  MkRandom -> lobby
  MkAddBot
    | length (theirs lobby) < mostBots -> lobby {theirs = theirs lobby <> [Nothing]}
    | otherwise -> lobby
  MkDropBot
    | length (theirs lobby) > 1 ->
        let kept = take (length (theirs lobby) - 1) (theirs lobby)
         in lobby {theirs = kept, choosing = if choosing lobby == MkTheirs (length kept) then MkMine else choosing lobby, invites = Map.delete (length kept) (invites lobby)}
    | otherwise -> lobby
  MkStart -> lobby
  MkOpenImport -> lobby {importing = Just (MkImport Text.empty Text.empty [] False)}
  MkCloseImport -> lobby {importing = Nothing}
  MkImportName -> lobby {importing = fmap (\i -> i {importName = typed}) (importing lobby)}
  MkImportText -> lobby {importing = fmap (\i -> i {importText = typed}) (importing lobby)}
  MkSubmitImport -> lobby
  MkDelete _ -> lobby
  MkExport _ -> lobby
  MkCloseExport -> lobby {exporting = Nothing}
  MkOpenBuilder -> lobby
  MkEdit _ -> lobby
  MkInviteSlot i
    | i >= 0 && i < length (theirs lobby) -> lobby {invites = Map.insertWith (\_ old -> old) i newInvite (invites lobby), choosing = if choosing lobby == MkTheirs i then MkMine else choosing lobby}
    | i == length (theirs lobby) && i < mostBots -> updated event typed lobby {theirs = theirs lobby <> [Nothing]}
    | otherwise -> lobby
  MkBotSlot i -> lobby {invites = Map.delete i (invites lobby)}
  MkReplyCode i -> lobby {invites = Map.adjust (\invite -> invite {replyCode = typed}) i (invites lobby)}
  MkConnect _ -> lobby
  MkReady -> lobby {ready = not (ready lobby)}

-- | The lobby once the page of the player invited to this slot said this
-- (Parun.Web.Link): their deck, and whether they are ready.
heard :: Int -> Link.Message -> Lobby -> Lobby
heard i message lobby =
  let adjust f = lobby {invites = Map.adjust f i (invites lobby)}
   in settled lobby $ case message of
        Link.MkHello name contents -> adjust (\invite -> invite {guestDeck = Just (name, contents)})
        Link.MkReady on -> adjust (\invite -> invite {guestReady = on})
        _ -> lobby

-- | An invited slot's link made, connected, or ended, saying why.
linked :: Int -> (Invite -> Invite) -> Lobby -> Lobby
linked i f lobby = settled lobby lobby {invites = Map.adjust f i (invites lobby)}

-- | The lobby after a change, every ready flag cleared where the match-up
-- changed: a deck, a seat or an invite.
settled :: Lobby -> Lobby -> Lobby
settled before after
  | matchupOf before == matchupOf after = after
  | otherwise = unready after
  where
    matchupOf l = (mine l, theirs l, fmap (\invite -> (connected invite, fmap fst (guestDeck invite), fmap snd (guestDeck invite))) (invites l))

-- | The lobby with every ready flag cleared.
unready :: Lobby -> Lobby
unready lobby = lobby {ready = False, invites = fmap (\invite -> invite {guestReady = False}) (invites lobby)}

-- | The lobby a Rematch of a game with players invited returns to: the
-- decks played, your own first, each bot's slot in `invited` held by its
-- player, still connected over the channel the game used, and nobody ready.
rematch :: NonEmpty.NonEmpty (Text.Text, Text.Text) -> Set.Set Int -> Lobby
rematch (yours NonEmpty.:| rest) invited =
  new
    { mine = Just (fst yours),
      theirs = [if Set.member i invited then Nothing else Just name | (i, (name, _)) <- zip [0 ..] rest],
      invites = Map.fromList [(i, newInvite {connected = True, guestDeck = Just seat}) | (i, seat) <- zip [0 ..] rest, Set.member i invited]
    }

-- | Whether a game with the players invited starts: there is one, your
-- deck is chosen, and every player is connected, has a deck and is ready.
starts :: Lobby -> Bool
starts lobby =
  not (Map.null (invites lobby))
    && ready lobby
    && Maybe.isJust (mine lobby)
    && all (\invite -> connected invite && Maybe.isJust (guestDeck invite) && guestReady invite) (invites lobby)

-- | The match-up as the player invited to this slot is shown it.
placesFor :: Int -> Lobby -> [Link.Place]
placesFor i lobby =
  Link.MkPlace (Text.pack "Host") (maybe (Text.pack "Choosing a deck") label (mine lobby)) (Just (ready lobby))
    : [ case Map.lookup j (invites lobby) of
          Just invite -> Link.MkPlace (if j == i then Text.pack "You" else Text.pack ("Player " <> show (j + 2))) (maybe (Text.pack "Choosing a deck") (label . fst) (guestDeck invite)) (Just (guestReady invite))
          Nothing -> Link.MkPlace (Text.pack (if length (theirs lobby) == 1 then "Bot" else "Bot " <> show (j + 1))) (maybe (Text.pack "Random deck") label held) Nothing
      | (j, held) <- zip [0 ..] (theirs lobby)
      ]

-- | Your deck, to start a game with; or why it may not: the problems a
-- check found with it, or none while there is no deck.
startable :: Lobby -> Either [Text.Text] Text.Text
startable lobby = case mine lobby of
  Nothing -> Left []
  Just name -> maybe (Right name) Left (Map.lookup name (illegal lobby))

-- | The deck chosen for a slot, if any.
chosen :: Lobby -> Slot -> Maybe Text.Text
chosen lobby slot = case slot of
  MkMine -> mine lobby
  MkTheirs i -> Monad.join (Maybe.listToMaybe (drop i (theirs lobby)))

-- | A deck's set and its own name: "sets/m12/entangling-webs" is set "M12",
-- deck "Entangling Webs".
describe :: Text.Text -> (Text.Text, Text.Text)
describe name =
  let parts = Text.splitOn (Text.pack "/") name
      (group, leaf) = case reverse parts of
        l : s : _ -> (Text.toUpper s, l)
        [l] -> (Text.pack "Decks", l)
        [] -> (Text.pack "Decks", name)
   in (group, titled leaf)

-- | "entangling-webs" as "Entangling Webs".
titled :: Text.Text -> Text.Text
titled = Text.unwords . fmap capital . Text.split (\c -> c == '-' || c == '_')
  where
    capital w = case Text.uncons w of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> w

-- | The deck as the match-up names it: its own name and its set.
label :: Text.Text -> Text.Text
label name = let (group, deck) = describe name in deck <> Text.pack " (" <> group <> Text.pack ")"

-- | Whether the deck matches what is typed, by its path or its name, any case.
matches :: Text.Text -> Text.Text -> Bool
matches typed name =
  let needle = Text.toCaseFold (Text.strip typed)
   in Text.null needle || Text.isInfixOf needle (Text.toCaseFold name) || Text.isInfixOf needle (Text.toCaseFold (label name))

-- | The decks the seat being chosen for may play: every deck for yours, its
-- 'opponents' for a bot's.
offered :: Lobby -> Seq.Seq Text.Text
offered lobby = if choosing lobby == MkMine then decks lobby else opponents lobby

-- | The decks offered on the shelf shown.
shelved :: Lobby -> [Text.Text]
shelved lobby = filter ((== (shelf lobby == MkSaved)) . isSaved) (Foldable.toList (offered lobby))

-- | The decks shown: those shelved that match what is typed, hold every
-- colour toggled on (or are still to have their colours read), and, among
-- the shipped, are of the set chosen.
shown :: Lobby -> [Text.Text]
shown lobby =
  let coloured name = maybe True (Set.isSubsetOf (colourFilter lobby)) (Map.lookup name (palette lobby))
      ofSet name = shelf lobby == MkSaved || all (== fst (describe name)) (setFilter lobby)
   in filter (\name -> matches (search lobby) name && coloured name && ofSet name) (shelved lobby)

-- | The sets the shipped decks come from, by code, in order.
setsOf :: Lobby -> [Text.Text]
setsOf lobby = Set.toList (Set.fromList [fst (describe name) | name <- Foldable.toList (decks lobby), not (isSaved name)])

-- | The bot's slot Invite a player gives to a player from another browser:
-- the first bot's left to a random deck, else a new seat while there is
-- room.
inviteTarget :: Lobby -> Maybe Int
inviteTarget lobby =
  case [i | (i, Nothing) <- zip [0 ..] (theirs lobby), Map.notMember i (invites lobby)] of
    i : _ -> Just i
    []
      | length (theirs lobby) < mostBots -> Just (length (theirs lobby))
      | otherwise -> Nothing

-- | A seat's name: "You", "Bot" alone or "Bot 2" among several, and
-- "Player 2" for one invited.
seatName :: Lobby -> Slot -> String
seatName lobby slot = case slot of
  MkMine -> "You"
  MkTheirs i
    | Map.member i (invites lobby) -> "Player " <> show (i + 2)
    | length (theirs lobby) == 1 -> "Bot"
    | otherwise -> "Bot " <> show (i + 1)

-- | What stops Start, or Ready with players invited: the decks unread, or
-- your deck unchosen or breaking its format. A bot always has a deck to
-- draw, since your deck is among its 'opponents'.
blocking :: Lobby -> Maybe String
blocking lobby
  | reading lobby = Just "Reading the decks\x2026"
  | Maybe.isNothing (mine lobby) = Just "Choose your deck to start."
  | Left (_ : _) <- startable lobby = Just "Your deck breaks its format:"
  | otherwise = Nothing

-- | Why the game may not start yet: what is 'blocking', else who invited
-- is still to connect, choose or be ready, which Ready does not wait on.
waiting :: Lobby -> Maybe String
waiting lobby = case blocking lobby of
  Just why -> Just why
  Nothing -> case [(i, invite) | (i, invite) <- Map.toList (invites lobby), not (connected invite && Maybe.isJust (guestDeck invite) && guestReady invite)] of
    (i, invite) : _
      | not (connected invite) -> Just (seatName lobby (MkTheirs i) <> " is still to connect.")
      | Maybe.isNothing (guestDeck invite) -> Just (seatName lobby (MkTheirs i) <> " is choosing a deck.")
      | otherwise -> Just (seatName lobby (MkTheirs i) <> " is not ready yet.")
    [] -> Nothing

-- | Whether Start, or Ready with players invited, may be pressed.
pressable :: Lobby -> Bool
pressable = Maybe.isNothing . blocking

-- | A colour's name.
colourName :: Color.Color -> String
colourName = show

-- | Colours as a deck's detail names them.
colourWords :: Set.Set Color.Color -> String
colourWords colours
  | Set.null colours = "Colourless"
  | otherwise = List.intercalate ", " (fmap colourName (Set.toList colours))

-- | A deck's colours as mana symbols.
pips :: Set.Set Color.Color -> Html.Html msg
pips colours = Element.el "span" ["pips"] [] [Mana.symbol (Text.pack (Web.Card.letterUpper c)) | c <- Set.toList colours]

view :: Lobby -> Html.Html Event
view lobby =
  Element.keyed
    "main"
    ["lobby", "new-game"]
    []
    [ (Text.pack "seats", seatsView lobby),
      (Text.pack "browser", browserView lobby),
      (Text.pack "detail", detailView lobby)
    ]

-- | The seats: yours, each bot's or invited player's, the ways to add one,
-- and Start at the foot, saying what it waits on.
seatsView :: Lobby -> Html.Html Event
seatsView lobby =
  Element.el "section" ["seats"] [Element.attr "aria-label" (Text.pack "Seats")] $
    [ Element.el "h1" [] [] [Element.string "New game"],
      Element.el "span" ["eyebrow"] [] [Element.string "At the table"],
      seat MkMine (Element.string "Y") (mine lobby) []
    ]
      <> concat
        [ Element.el "span" ["eyebrow", "versus"] [] [Element.string "versus"]
            : case Map.lookup i (invites lobby) of
              Just invite -> [inviteView i invite (remove i)]
              Nothing -> [seat (MkTheirs i) (Icon.icon [] Icon.MkBot) held (Element.button ["quiet", "invite-seat"] (MkInviteSlot i) [Element.string "Invite a player instead"] : remove i)]
        | (i, held) <- zip [0 ..] bots
        ]
      <> [ Element.el
             "div"
             ["seat-tools"]
             []
             ( [Element.button ["add-bot"] MkAddBot [Element.string "Add a bot"] | length bots < mostBots]
                 <> [Element.button ["invite-slot"] (MkInviteSlot i) [Element.string "Invite a player"] | i <- Maybe.maybeToList (inviteTarget lobby)]
             ),
           startView lobby
         ]
  where
    bots = theirs lobby
    -- Only the last seat is unseated: the invites' links are numbered by
    -- seat, so unseating another would renumber a live connection.
    remove i = [Element.button ["quiet", "drop-bot"] MkDropBot [Element.string "Remove"] | i == length bots - 1, length bots > 1]
    seat which avatar held extra =
      let on = choosing lobby == which
       in Element.el
            "div"
            (["lobby-seat"] <> ["on" | on])
            []
            $ Element.el
              "button"
              ["seat-pick"]
              [Element.attr "type" (Text.pack "button"), Element.on "click" (MkChoose which), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false"))]
              [ Element.el
                  "span"
                  ["seat-head"]
                  []
                  ( [Element.el "span" ["avatar"] [] [avatar], Element.el "strong" ["seat-name"] [] [Element.string (seatName lobby which)]]
                      <> [Element.el "span" ["chip", "choosing"] [] [Element.string "Choosing a deck"] | on]
                  ),
                Element.el "span" ["seat-deck"] [] $ case held of
                  Just name -> [pips (Map.findWithDefault Set.empty name (palette lobby)), Element.el "span" ["seat-deck-name"] [] [Element.text (snd (describe name))]]
                  Nothing -> [Element.el "span" ["seat-deck-name"] [] [Element.string (if which == MkMine then "No deck yet" else "Random deck")]],
                Element.el "span" ["seat-note", "muted"] [] [Element.string (maybe (if which == MkMine then "Choose one to start" else "Drawn when the game starts") (counted lobby) held)]
              ]
              : [Element.el "div" ["seat-actions"] [] extra | not (null extra)]

-- | A deck's set and its card count, as far as read: "M12 · 60 cards".
counted :: Lobby -> Text.Text -> String
counted lobby name =
  List.intercalate " \x00b7 " ([Text.unpack (fst (describe name)) | not (isSaved name)] <> ["Saved" | isSaved name] <> [show n <> " cards" | n <- Maybe.maybeToList (Map.lookup name (sizes lobby))])

-- | Start, or Ready with players invited, and what it waits on.
startView :: Lobby -> Html.Html Event
startView lobby =
  Element.el "div" ["lobby-start"] [] $
    [ if Map.null (invites lobby)
        then
          Element.el
            "button"
            ["primary", "start"]
            ([Element.attr "type" (Text.pack "button"), Element.on "click" MkStart, Element.attr "aria-describedby" (Text.pack "start-note")] <> [Element.attr "disabled" Text.empty | not (pressable lobby)])
            [Element.string "Start game", Element.el "kbd" ["keys-only"] [Element.attr "aria-hidden" (Text.pack "true")] [Element.string "\x21b5"]]
        else
          -- The game starts once every player is ready.
          Element.el
            "button"
            (["primary", "start", "ready"] <> ["is-ready" | ready lobby])
            ([Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if ready lobby then "true" else "false")), Element.on "click" MkReady, Element.attr "aria-describedby" (Text.pack "start-note")] <> [Element.attr "disabled" Text.empty | not (pressable lobby)])
            [Element.string (if ready lobby then "Ready \x2713 \x00b7 waiting for every player" else "Ready")],
      Element.el "p" ["start-note", "muted"] [Element.attr "id" (Text.pack "start-note"), Element.attr "aria-live" (Text.pack "polite")] [Element.string (Maybe.fromMaybe "Every seat has a deck. Ready to start." (waiting lobby))]
    ]
      <> [Element.el "ul" ["problems", "refusal"] [] [Element.el "li" [] [] [Element.text p] | p <- problems] | Left problems@(_ : _) <- [startable lobby]]

-- | The decks the seat being chosen for may play: the ways to add one, the
-- shelves, the filters, and the decks shown as tiles.
browserView :: Lobby -> Html.Html Event
browserView lobby =
  Element.keyed "section" ["browser"] [Element.attr "aria-label" (Text.pack "Decks")] $
    [ ( Text.pack "head",
        Element.el
          "div"
          ["browser-head"]
          []
          [ Element.el "h2" [] [] [Element.string ("Deck for " <> seatName lobby (choosing lobby))],
            Element.button ["open-import"] MkOpenImport [Element.string "Import a deck"],
            Element.button ["open-builder"] MkOpenBuilder [Element.string "Build a deck"]
          ]
      )
    ]
      <> foldMap (pure . (,) (Text.pack "import") . importView) (importing lobby)
      <> [ ( Text.pack "shelves",
             Element.el
               "div"
               ["shelves"]
               [Element.attr "role" (Text.pack "tablist"), Element.attr "aria-label" (Text.pack "Which decks")]
               [tab MkPrecons "Precons", tab MkSaved "My decks"]
           ),
           ( Text.pack "filters",
             Element.el
               "div"
               ["deck-tools"]
               []
               [ Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-search")] [Element.string "Filter decks"],
                 Element.el "input" ["search"] [Element.attr "id" (Text.pack "deck-search"), Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter decks or sets"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkSearch] [],
                 Element.el "div" ["colour-toggles"] [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Colours")] (fmap toggle [minBound .. maxBound]),
                 Element.el "label" ["sr-only"] [Element.attr "for" (Text.pack "deck-set")] [Element.string "Set"],
                 Element.el
                   "select"
                   ["set-filter"]
                   ([Element.attr "id" (Text.pack "deck-set"), Element.on "change" MkSet] <> [Element.attr "disabled" Text.empty | shelf lobby == MkSaved])
                   (option Text.empty (Text.pack "All sets") : [option code code | code <- setsOf lobby]),
                 Element.button ["random"] MkRandom [Element.string "Surprise me"]
               ]
           ),
           ( Text.pack "decks",
             Element.el "div" ["deck-results"] [] $ case problem lobby of
               Just p -> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p]]
               Nothing
                 | reading lobby -> [Element.el "p" ["empty"] [] [Element.string "Reading the decks\x2026"]]
                 | null listed -> [Element.el "p" ["empty"] [] [Element.string (if shelf lobby == MkSaved && null (shelved lobby) then "No saved decks yet: build or import one." else "No deck matches.")]]
                 | otherwise ->
                     [ Element.keyed "div" ["deck-grid"] [] (fmap tile listed),
                       Element.el "p" ["deck-count", "muted"] [] [Element.string (if length listed == length (shelved lobby) then show (length listed) <> " decks" else show (length listed) <> " of " <> show (length (shelved lobby)) <> " decks")]
                     ]
           )
         ]
  where
    listed = shown lobby
    held = chosen lobby (choosing lobby)
    tab which title =
      let on = shelf lobby == which
       in Element.el
            "button"
            (["tab"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "role" (Text.pack "tab"), Element.attr "aria-selected" (Text.pack (if on then "true" else "false")), Element.on "click" (MkShelf which)]
            [Element.string title]
    toggle c =
      let on = Set.member c (colourFilter lobby)
       in Element.el
            "button"
            (["toggle"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (Text.pack (colourName c)), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false")), Element.on "click" (MkColour c)]
            [Mana.symbol (Text.pack (Web.Card.letterUpper c))]
    option value' label' = Element.el "option" [] ([Element.attr "value" value'] <> [Element.attr "selected" Text.empty | setFilter lobby == if Text.null value' then Nothing else Just value']) [Element.text label']
    tile name =
      let picked = held == Just name
          (code, own) = describe name
       in ( name,
            Element.el
              "button"
              (["deck"] <> ["picked" | picked])
              [Element.attr "type" (Text.pack "button"), Element.on "click" (MkPick name), Element.attr "aria-pressed" (Text.pack (if picked then "true" else "false"))]
              ( [ Element.el "span" ["deck-top"] [] ([Element.el "span" ["set-badge"] [] [Element.text code] | not (isSaved name)] <> [pips (Map.findWithDefault Set.empty name (palette lobby))]),
                  Element.el "span" ["deck-name"] [] [Element.text own]
                ]
                  <> [Element.el "span" ["deck-badges"] [] (format name) | not (null (format name))]
                  <> [curve ["spark"] counts | counts <- Maybe.maybeToList (Map.lookup name (curves lobby))]
              )
          )
    format name = [Element.el "span" ["deck-format"] [] [Element.text (titled f)] | f <- Maybe.maybeToList (Map.lookup name (formats lobby))] <> [Element.el "span" ["not-legal"] [] [Element.string "Not legal"] | Map.member name (illegal lobby)]

-- | The deck of the seat being chosen for: its name, set, size and colours,
-- its curve and kinds of card, its list, and the ways to edit it, and a
-- saved one's to copy it out and delete it.
detailView :: Lobby -> Html.Html Event
detailView lobby =
  Element.el "aside" ["detail"] [Element.attr "aria-label" (Text.pack "Deck chosen")] $
    Element.el "span" ["eyebrow"] [] [Element.string (if choosing lobby == MkMine then "Your deck" else seatName lobby (choosing lobby) <> "'s deck")]
      : case chosen lobby (choosing lobby) of
        Nothing ->
          [ Element.el "h2" [] [] [Element.string (if choosing lobby == MkMine then "No deck yet" else "Random deck")],
            Element.el "p" ["muted"] [] [Element.string (if choosing lobby == MkMine then "Pick one of the decks to see it here." else "A deck sharing your deck's format, drawn when the game starts. Pick one to choose it.")]
          ]
        Just name ->
          [ Element.el "div" ["detail-name"] [] [pips (Map.findWithDefault Set.empty name (palette lobby)), Element.el "h2" [] [] [Element.text (snd (describe name))]],
            Element.el "p" ["muted"] [] [Element.string (List.intercalate " \x00b7 " ([counted lobby name] <> [colourWords colours | Map.member name (palette lobby)] <> [Text.unpack (titled f) | f <- Maybe.maybeToList (Map.lookup name (formats lobby))]))]
          ]
            <> case Map.lookup name (previews lobby) of
              Nothing -> [Element.el "p" ["empty"] [] [Element.string "Reading the list\x2026"]]
              Just lines' ->
                [ Element.el "div" ["panel", "detail-curve"] [] [Element.el "span" ["eyebrow"] [] [Element.string "Mana curve"], curve [] (curveOf lines')],
                  Element.el "dl" ["panel", "kinds"] [] [Element.el "div" [] [] [Element.el "dt" [] [] [Element.string title], Element.el "dd" ["muted"] [] [Element.string (show (sum [copies l | l <- lines', kind l == k]))]] | (title, k) <- kinds],
                  Element.el "details" ["deck-preview"] [] [Element.el "summary" [] [] [Element.string ("See all " <> show (sum (fmap copies lines')) <> " cards")], cardList lines']
                ]
            <> [ Element.el "div" ["saved-tools"] [] $
                   if isSaved name
                     then [Element.button ["edit"] (MkEdit name) [Element.string "Edit"], Element.button ["export"] (MkExport name) [Element.string "Export"], Element.button ["delete"] (MkDelete name) [Element.string "Delete"]]
                     else [Element.button ["edit"] (MkEdit name) [Element.string "Edit a copy"]]
               ]
            <> [exportView shown' | shown'@(exported, _) <- Maybe.maybeToList (exporting lobby), exported == name]
          where
            colours = Map.findWithDefault Set.empty name (palette lobby)

-- | A slot a player from another browser takes: the link to send them, the
-- box their reply code goes in, and once connected their deck and whether
-- they are ready; the warning that the host sees every card; the way back
-- to a bot; and `extra`, the way to unseat it where there is one.
inviteView :: Int -> Invite -> [Html.Html Event] -> Html.Html Event
inviteView i invite extra =
  Element.el
    "div"
    ["lobby-seat", "invite"]
    [Element.attr "aria-label" (Text.pack ("Invited player, seat " <> show (i + 2)))]
    $ [ Element.el
          "div"
          ["seat-pick", "invited"]
          []
          [ Element.el
              "span"
              ["seat-head"]
              []
              ( [Element.el "span" ["avatar"] [] [Element.string "P"], Element.el "strong" ["seat-name"] [] [Element.string ("Player " <> show (i + 2))]]
                  <> [Element.el "span" ["chip"] [] [Element.string (if guestReady invite then "Ready \x2713" else "Not ready")] | connected invite]
              ),
            Element.el "span" ["seat-deck"] [] [Element.el "span" ["seat-deck-name"] [] [Element.text (maybe (Text.pack (if connected invite then "Choosing a deck\x2026" else "Not connected")) (snd . describe . fst) (guestDeck invite))]],
            Element.el "span" ["seat-note", "muted"] [] [Element.string (if connected invite then "Chooses their own deck" else "Invited from another browser")]
          ]
      ]
      <> ( if connected invite
             then []
             else case link invite of
               Nothing -> [Element.el "p" ["invite-step"] [] [Element.string "Making the link\x2026"]]
               Just url ->
                 [ Element.el "label" [] [Element.attr "for" (field "invite-link")] [Element.string "1. Send them this link"],
                   Element.el "input" ["invite-link"] [Element.attr "id" (field "invite-link"), Element.attr "type" (Text.pack "text"), Element.attr "readonly" Text.empty, Element.attr "value" url] [],
                   Element.el "label" [] [Element.attr "for" (field "invite-reply")] [Element.string "2. Paste the code they send back"],
                   Element.el "input" ["invite-reply"] [Element.attr "id" (field "invite-reply"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "spellcheck" (Text.pack "false"), Element.on "input" (MkReplyCode i)] [],
                   Element.el "button" ["connect"] ([Element.attr "type" (Text.pack "button"), Element.on "click" (MkConnect i)] <> [Element.attr "disabled" Text.empty | Text.null (Text.strip (replyCode invite))]) [Element.string "Connect"]
                 ]
         )
      <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (inviteProblem invite)]
      <> [ Element.el "p" ["invite-warning"] [] [Element.string "Your browser runs the game, so you could see their hidden cards: invite only friends."],
           Element.el "div" ["seat-actions"] [] (Element.button ["quiet", "bot-slot"] (MkBotSlot i) [Element.string "Play a bot instead"] : extra)
         ]
  where
    field name = Text.pack (name <> "-" <> show i)

-- | The panel a deck list is pasted into: its name, its list, and why the
-- last import was refused.
importView :: Import -> Html.Html Event
importView panel =
  Element.el
    "section"
    ["import"]
    [Element.attr "aria-label" (Text.pack "Import a deck")]
    $ [ Element.el "h2" [] [] [Element.string "Import a deck"],
        Element.el "label" [] [Element.attr "for" (Text.pack "import-name")] [Element.string "Name"],
        Element.el "input" ["import-name"] [Element.attr "id" (Text.pack "import-name"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.on "input" MkImportName] [],
        Element.el "label" [] [Element.attr "for" (Text.pack "import-text")] [Element.string "Deck list"],
        Element.el "textarea" ["import-text"] [Element.attr "id" (Text.pack "import-text"), Element.attr "rows" (Text.pack "12"), Element.attr "spellcheck" (Text.pack "false"), Element.attr "placeholder" (Text.pack "Format: constructed\n24 Forest\n4 Llanowar Elves"), Element.on "input" MkImportText] []
      ]
      <> [Element.el "ul" ["problems"] [Element.attr "role" (Text.pack "alert")] [Element.el "li" [] [] [Element.text p] | p <- importProblems panel] | not (null (importProblems panel))]
      <> [ Element.el
             "div"
             ["import-tools"]
             []
             [ Element.el "button" ["primary"] ([Element.attr "type" (Text.pack "button"), Element.on "click" MkSubmitImport] <> [Element.attr "disabled" Text.empty | checking panel]) [Element.string (if checking panel then "Checking\x2026" else "Import")],
               Element.button [] MkCloseImport [Element.string "Cancel"]
             ]
         ]

-- | A saved deck's list, to copy.
exportView :: (Text.Text, Text.Text) -> Html.Html Event
exportView (name, contents) =
  Element.el
    "section"
    ["export"]
    [Element.attr "aria-label" (Text.pack "Export a deck")]
    [ Element.el "h2" [] [] [Element.text (snd (describe name))],
      Element.el "textarea" ["export-text"] [Element.attr "readonly" Text.empty, Element.attr "rows" (Text.pack "12"), Element.attr "aria-label" (Text.pack "Deck list")] [Element.text contents],
      Element.button [] MkCloseExport [Element.string "Close"]
    ]
