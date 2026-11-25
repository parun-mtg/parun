-- | The web client's lobby (#9421): the seats at the table, the seat being
-- chosen for, which a tap on a seat or a seat added changes, the game's
-- options, and Start at their foot; the decks that seat may play, filtered
-- by what is typed, by colour, by set and by whether they are saved; and the
-- chosen deck's detail.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * Deploying creatures, the multiplayer variants but Free-for-All (CR 804,
--     807 to 811) and Archenemy (CR 904) (#9749).
module Parun.Web.Lobby where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable
import Numeric.Natural (Natural)
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.Card as EngineCard
import qualified Parun.Engine.RuleFamily as RuleFamily.Engine
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Types.AttackOption as AttackOption
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.RangeOfInfluence as RangeOfInfluence
import qualified Parun.Types.RuleFamily as RuleFamily
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.TypeLine as TypeLine
import qualified Parun.Web.BotSettings as BotSettings
import qualified Parun.Web.Clock as Clock
import qualified Parun.Web.DeckBrowser as DeckBrowser
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Link as Link
import qualified Text.Read as Read

data Lobby = MkLobby
  { -- | The decks, what is read of them, and what they are filtered by.
    browser :: DeckBrowser.DeckBrowser,
    -- | The player's deck, once chosen.
    mine :: Maybe Text.Text,
    -- | Each other seat's deck, in seat order, once chosen; a random one
    -- otherwise. There are at most 'mostOthers' such seats, and none is a
    -- game of yours alone (goldfish).
    theirs :: [Maybe Text.Text],
    -- | The seat a tapped deck goes to, which a tap on a seat or a seat
    -- added changes, never a pick.
    choosing :: Slot,
    -- | Each seat taken by a player invited from another browser, by its
    -- place in 'theirs' (Parun.Web.Link).
    invites :: Map.Map Int Invite,
    -- | The link number the next invite takes, never one an invite took
    -- before, so a closed link's late word reaches no other.
    nextLink :: Int,
    -- | Each seat played on this screen, by its place in 'theirs', which
    -- holds its deck, and how it is played there; a bot plays every seat in
    -- neither map.
    locals :: Map.Map Int Link.Sitting,
    -- | Whether you are ready to start a game with the players invited.
    ready :: Bool,
    -- | How strongly each bot plays and what it values in a position
    -- (Parun.Play.Opponent), by its place in 'theirs'; 'BotSettings.defaultBot' in a
    -- place absent ('botAt').
    bots :: Map.Map Int (Difficulty.Difficulty, Persona.Persona),
    -- | Whether a game is in progress, set aside for this lobby, which
    -- starting another abandons.
    paused :: Bool,
    -- | What was tapped that abandons the game in progress, asked about
    -- first.
    abandoning :: Maybe Event,
    -- | The options the game is started with.
    options :: Options,
    -- | Why Start last refused the seats' lists ('planarDealt'), until the
    -- match-up changes.
    refusal :: Maybe Text.Text
  }
  deriving (Eq, Show)

-- | The options of CR 800.2, and the variants' (CR 407, 901), a game started
-- here is played with ('settings'): those no team plays (CR 808.1).
data Options = MkOptions
  { -- | CR 407.1: whether the game is played for ante.
    ante :: Bool,
    -- | CR 901: whether the game is a Planechase game, each seat whose list
    -- brings no planar deck dealt one ('planarDealt').
    planechase :: Bool,
    -- | CR 901.15: whether a Planechase game uses one communal planar deck.
    singlePlanarDeck :: Bool,
    -- | CR 806.2b: the attack option a game of three or more seats uses.
    attackOption :: AttackOption.AttackOption,
    -- | CR 806.2a: every player's range of influence in a game of three or
    -- more seats, in seats; unlimited where absent (CR 801.1).
    rangeOfInfluence :: Maybe Natural,
    -- | The minutes each seat a person plays has on its clock
    -- (Parun.Web.Clock), which no rule sets; untimed where absent.
    clock :: Maybe Natural
  }
  deriving (Eq, Show)

-- | No option in use: CR 802's attack multiple players, which needs no
-- seating agreed ('GameSettings.plain').
noOptions :: Options
noOptions = MkOptions {ante = False, planechase = False, singlePlanarDeck = False, attackOption = AttackOption.MultiplePlayers, rangeOfInfluence = Nothing, clock = Nothing}

-- | The attack options a game may choose (CR 806.2b), each with its value
-- and its label; CR 809.3c's is the Emperor variant's.
attackOptions :: [(AttackOption.AttackOption, (String, String))]
attackOptions =
  [ (AttackOption.MultiplePlayers, ("multiple", "Attack any opponents")),
    (AttackOption.Leftward, ("left", "Attack left")),
    (AttackOption.Rightward, ("right", "Attack right"))
  ]

-- | The limited ranges of influence offered (CR 801.2a's commonest), each
-- with its value and label, after an unlimited one.
ranges :: [(Maybe Natural, (String, String))]
ranges = (Nothing, ("unlimited", "Unlimited")) : [(Just n, (show n, show n <> (if n == 1 then " seat" else " seats"))) | n <- [1, 2]]

-- | Whether a game of so many seats is multiplayer, and so chooses CR 806.2's
-- options (CR 800.1).
multiplayer :: Natural -> Bool
multiplayer count = count >= 3

-- | How many seats the game has: yours and each other.
seatCount :: Lobby -> Natural
seatCount lobby = 1 + Natural.length (theirs lobby)

-- | The options the game is started with (CR 800.2), its format aside, which
-- the lists' headers name: the lobby's, CR 806.2's at three seats or more
-- only, a range of influence being every seat's (CR 806.2a).
settings :: Lobby -> GameSettings.GameSettings
settings lobby =
  let chosen' = options lobby
      count = seatCount lobby
      everyone n = RangeOfInfluence.MkRangeOfInfluence (Map.fromList [(Command.idOf k, n) | k <- [1 .. count]])
   in GameSettings.plain
        { GameSettings.ante = ante chosen',
          GameSettings.singlePlanarDeck = planechase chosen' && singlePlanarDeck chosen',
          GameSettings.attackOption = if multiplayer count then Just (attackOption chosen') else GameSettings.attackOption GameSettings.plain,
          GameSettings.rangeOfInfluence = if multiplayer count then maybe RangeOfInfluence.unlimited everyone (rangeOfInfluence chosen') else RangeOfInfluence.unlimited
        }

-- | A slot a player from another browser takes.
data Invite = MkInvite
  { -- | The page's number for its link (Parun.Browser.linkHost), its own
    -- rather than its seat's, so unseating a seat before it keeps it.
    linkNumber :: Int,
    -- | The link to send them, once made.
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

-- | An invite just made over link number `n`: no link yet, nothing typed.
newInvite :: Int -> Invite
newInvite n = MkInvite n Nothing Text.empty False Nothing False Nothing

-- | The seat whose invite has link number `n`.
seatOfLink :: Int -> Lobby -> Maybe Int
seatOfLink n lobby = Maybe.listToMaybe [i | (i, invite) <- Map.toList (invites lobby), linkNumber invite == n]

-- | A side of the match-up.
data Slot
  = MkMine
  | -- | The seat at this place in 'theirs', from zero.
    MkTheirs Int
  deriving (Eq, Show)

-- | Who plays a seat other than yours, chosen as it is added.
data Occupant
  = -- | The search bot (Parun.Play.Opponent).
    MkBot
  | -- | A player invited from another browser (Parun.Web.Link).
    MkInvited
  | -- | A seat played on this screen, as the sitting says.
    MkHere Link.Sitting
  deriving (Eq, Show)

-- | What a tap or a keystroke on the lobby does.
data Event
  = -- | Makes this deck the chosen slot's.
    MkPick Text.Text
  | -- | Chooses a deck for this slot next.
    MkChoose Slot
  | -- | Something done in the deck browser.
    MkBrowse DeckBrowser.Event
  | -- | A random deck of those shown, for the slot being chosen; the shell
    -- picks it.
    MkRandom
  | -- | Adds a seat past the last, up to 'mostOthers', played by this
    -- occupant, with a random deck, and chooses for it unless it is
    -- invited; the shell makes an invite's link, numbered 'nextLink'.
    MkAdd Occupant
  | -- | Unseats the seat at this place in 'theirs', those after it moving
    -- up; the shell closes an invite's link.
    MkDrop Int
  | -- | Starts the game.
    MkStart
  | -- | Opens the deck builder; the shell does it.
    MkOpenBuilder
  | -- | Opens this deck in the builder, a saved one to save over, any other
    -- as a copy; the shell does it.
    MkEdit Text.Text
  | -- | The reply code typed for this invited slot.
    MkReplyCode Int
  | -- | Connects this invited slot with the reply code typed; the shell
    -- does it.
    MkConnect Int
  | -- | Toggles whether you are ready to start with the players invited.
    MkReady
  | -- | Sets the difficulty of the bot at this place in 'theirs' to the one
    -- whose name is chosen.
    MkDifficulty Int
  | -- | Sets the persona of the bot at this place in 'theirs' to the one
    -- whose name is chosen.
    MkPersona Int
  | -- | Abandons the game in progress for what was asked about; the shell
    -- does it.
    MkAbandon
  | -- | Returns to the game in progress; the shell does it.
    MkResume
  | -- | Keeps the game in progress, and this lobby.
    MkCancel
  | -- | Toggles whether the game is played for ante (CR 407.1).
    MkAnte
  | -- | Toggles whether the game is a Planechase game (CR 901).
    MkPlanechase
  | -- | Toggles whether a Planechase game uses one planar deck (CR 901.15).
    MkSinglePlanarDeck
  | -- | Sets the attack option to the one whose value is chosen
    -- ('attackOptions').
    MkAttackOption
  | -- | Sets the range of influence to the one whose value is chosen
    -- ('ranges').
    MkRangeOfInfluence
  | -- | Sets each seat's time to the one whose value is chosen
    -- (Clock.choices).
    MkClock
  deriving (Eq, Show)

-- | A lobby whose decks are still to read, nothing chosen yet, your seat
-- alone, and any bot added playing as `parun play`'s does unless told
-- otherwise.
new :: Lobby
new =
  MkLobby
    { browser = DeckBrowser.new,
      mine = Nothing,
      theirs = [],
      choosing = MkMine,
      invites = Map.empty,
      nextLink = 0,
      locals = Map.empty,
      ready = False,
      bots = Map.empty,
      paused = False,
      abandoning = Nothing,
      options = noOptions,
      refusal = Nothing
    }

-- | A new lobby with this deck yours, on the shelf it is on.
holding :: Text.Text -> Lobby
holding name = browsed (\b -> b {DeckBrowser.shelf = DeckBrowser.shelfOf name}) new {mine = Just name}

-- | How the bot at this place in 'theirs' plays, of these 'bots'.
botAt :: Int -> Map.Map Int (Difficulty.Difficulty, Persona.Persona) -> (Difficulty.Difficulty, Persona.Persona)
botAt = Map.findWithDefault BotSettings.defaultBot

-- | The lobby with its deck browser changed.
browsed :: (DeckBrowser.DeckBrowser -> DeckBrowser.DeckBrowser) -> Lobby -> Lobby
browsed f lobby = lobby {browser = f (browser lobby)}

-- | Every deck.
decks :: Lobby -> Seq.Seq Text.Text
decks = DeckBrowser.decks . browser

-- | Whether a deck is one to play: not a set's whole card list.
playable :: Text.Text -> Bool
playable name = DeckBrowser.isSaved name || not (Text.isSuffixOf (Text.pack "/all") name)

-- | The lobby once this deck is imported: the panel closed, the saved decks
-- shown, and the deck the seat's being chosen for, or, where that bot may
-- not play it, yours, your seat then chosen so the chip shows where it went.
imported :: Text.Text -> Lobby -> Lobby
imported name lobby =
  let picked = update (MkPick name) Text.empty lobby
      seated = if chosen picked (choosing picked) == Just name then picked else update (MkPick name) Text.empty lobby {choosing = MkMine}
   in browsed DeckBrowser.imported seated

-- | The lobby without this deck, every seat holding it emptied.
deleted :: Text.Text -> Lobby -> Lobby
deleted name lobby =
  let emptied :: Maybe Text.Text -> Maybe Text.Text
      emptied = Monad.mfilter (/= name)
   in browsed (DeckBrowser.deleted name) lobby {mine = emptied (mine lobby), theirs = fmap emptied (theirs lobby)}

-- | The lobby holding these decks under these formats and these names, what
-- was chosen and typed kept.
withDecks :: Seq.Seq Text.Text -> Map.Map Text.Text Text.Text -> Map.Map Text.Text Text.Text -> Lobby -> Lobby
withDecks names named titled = browsed (DeckBrowser.withDecks names named titled)

-- | The most seats a game has besides yours: a table of four.
mostOthers :: Int
mostOthers = 3

-- | Of so many seats, the player's (`you`) and each in `people` speak the
-- line protocol; the search bot plays every other, at the difficulty and
-- persona (Parun.Play.Opponent) its place in 'theirs' has in `styles`
-- ('botAt'), the seats but the player's taking the places in seating order.
playOptions :: Map.Map Int (Difficulty.Difficulty, Persona.Persona) -> Natural -> PlayerId.PlayerId -> Set.Set PlayerId.PlayerId -> PlayOptions.PlayOptions
playOptions styles count you people =
  let seatPlay place = BotSettings.botPlay (botAt place styles)
      others = filter ((/= you) . Command.idOf) [1 .. count]
   in PlayOptions.none
        { PlayOptions.json = True,
          PlayOptions.bots = Map.fromList [(n, seatPlay place) | (place, n) <- zip [0 ..] others, Set.notMember (Command.idOf n) people]
        }

-- | Whether two decks are under one format, and so may meet: the game's
-- format is the one their headers name (Parun.Play.Command.settle).
together :: Lobby -> Text.Text -> Text.Text -> Bool
together lobby one two = case (Map.lookup one (DeckBrowser.formats (browser lobby)), Map.lookup two (DeckBrowser.formats (browser lobby))) of
  -- A Limited deck meets a deck naming no format that a Limited game takes,
  -- since the game is then Limited (Parun.Play.Command.gameFormat): the
  -- client's reading, so a sealed deck has opponents. A deck not yet
  -- counted is offered, and Start refuses it if it is too small.
  (Just named, Nothing) | named == limited -> bigEnough two
  (Nothing, Just named) | named == limited -> bigEnough one
  (mine', theirs') -> mine' == theirs'
  where
    limited = Text.pack "limited"
    bigEnough name = maybe True (>= RuleFamily.Engine.minimumDeckSize RuleFamily.Limited) (Map.lookup name (DeckBrowser.sizes (browser lobby)))

-- | The decks the bot may play against yours: every deck, until yours is
-- chosen.
opponents :: Lobby -> Seq.Seq Text.Text
opponents lobby = maybe (decks lobby) (\one -> Seq.filter (together lobby one) (decks lobby)) (mine lobby)

-- | The fewest cards a bot's random deck holds against your deck of this
-- many: as many, up to Limited's minimum, so a demo deck meets only its like.
fewestDealt :: Natural -> Natural
fewestDealt yours = min yours (RuleFamily.Engine.minimumDeckSize RuleFamily.Limited)

-- | A bot's deck dealt at random from these, `roll` picking one: one that
-- `keeps` refuses is dropped and the rest rolled again; nothing once none
-- is left.
dealt :: (Monad m) => (Seq.Seq Text.Text -> m (Maybe Text.Text)) -> (Text.Text -> m Bool) -> Seq.Seq Text.Text -> m (Maybe Text.Text)
dealt roll keeps among =
  let next name = keeps name >>= \kept -> if kept then pure (Just name) else dealt roll keeps (Seq.filter (/= name) among)
   in roll among >>= maybe (pure Nothing) next

-- | A card a planar deck may hold (CR 901.3): its name and whether it is a
-- phenomenon card; nothing for any other card, or a synthetic one.
planarCardOf :: Card.Card -> Maybe (CardName.CardName, Bool)
planarCardOf c =
  let face = EngineCard.combined c
      types = TypeLine.types (Face.typeLine face)
      phenomenon = Set.member CardType.Phenomenon types
   in if not (Card.synthetic c) && (phenomenon || Set.member CardType.Plane types) then Just (Face.name face, phenomenon) else Nothing

-- | These, in an order `pick` makes: `pick n` names one of the first `n`
-- left, from zero.
shuffledBy :: (Monad m) => (Int -> m Int) -> [a] -> m [a]
shuffledBy pick among = case among of
  [] -> pure []
  _ -> do
    i <- pick (length among)
    case splitAt i among of
      (before, chosen' : after) -> (chosen' :) <$> shuffledBy pick (before <> after)
      (before, []) -> pure before

-- | CR 901.3: the cards of a planar deck dealt from these, in order: the
-- first ten, a phenomenon past the second passed over; and the cards left,
-- in order. Nothing where too few are.
planarHand :: [(CardName.CardName, Bool)] -> Maybe ([CardName.CardName], [(CardName.CardName, Bool)])
planarHand = go [] (0 :: Int) []
  where
    go hand phenomena passed rest
      | length hand == 10 = Just (reverse hand, reverse passed <> rest)
      | otherwise = case rest of
          [] -> Nothing
          card@(name, phenomenon) : more
            | phenomenon && phenomena >= 2 -> go hand phenomena (card : passed) more
            | otherwise -> go (name : hand) (if phenomenon then phenomena + 1 else phenomena) passed more

-- | The names of the planar deck a deck list brings (CR 901.3).
planarNames :: Text.Text -> [CardName.CardName]
planarNames contents = [card | (DeckList.Supplementary SideDeck.Planar, _, card) <- sectioned contents]

-- | The seats' lists, each a deck's name and its list, as Start deals them.
-- With Planechase off, refused where one brings a planar deck.
-- With it on, each list bringing none given a planar deck (planarHand) from
-- `pool` in the order `shuffle` puts it: its own shuffle each, or, under CR
-- 901.15a's single planar deck, one shuffle shared, of the names no list
-- brings, so the communal deck repeats none. Refused, saying why, where the
-- pool runs short.
planarDealt :: (Monad m) => ([(CardName.CardName, Bool)] -> m [(CardName.CardName, Bool)]) -> Options -> [(CardName.CardName, Bool)] -> NonEmpty.NonEmpty (Text.Text, Text.Text) -> m (Either Text.Text (NonEmpty.NonEmpty (Text.Text, Text.Text)))
planarDealt shuffle chosen' pool seats
  | not (planechase chosen') = pure $ case [seat | seat <- NonEmpty.toList seats, brings seat] of
      seat@(name, _) : _ -> Left (Text.concat [Text.pack "Planechase is off, but ", DeckBrowser.label (DeckBrowser.titleIn seat) name, Text.pack " brings a planar deck: turn Planechase on, or drop its Planar Deck section."])
      [] -> Right seats
  | singlePlanarDeck chosen' = do
      let brought = Set.fromList (foldMap (planarNames . snd) seats)
          step left seat
            | brings seat = (left, Right seat)
            | otherwise = maybe (left, Left short) (\(hand, rest) -> (rest, Right (with hand seat))) (planarHand left)
      order <- shuffle [card | card@(name, _) <- pool, Set.notMember name brought]
      pure (sequence (snd (Traversable.mapAccumL step order seats)))
  | otherwise = sequence <$> traverse (\seat -> if brings seat then pure (Right seat) else maybe (Left short) (\(hand, _) -> Right (with hand seat)) . planarHand <$> shuffle pool) seats
  where
    brings = not . null . planarNames . snd
    with hand (name, contents) = (name, Text.unlines (Text.stripEnd contents : Text.empty : DeckList.heading (DeckList.Supplementary SideDeck.Planar) : [Text.pack "1 " <> CardName.unwrap card | card <- hand]))
    short = Text.pack ("The pool's " <> show (length pool) <> " planes and phenomena can't deal " <> (if singlePlanarDeck chosen' then "one planar deck for " <> show (length seats) <> " players" else "every seat a planar deck") <> ": turn the single planar deck off, or seat fewer players.")

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
mainEntries contents = [(n, card) | (DeckList.Main, n, card) <- sectioned contents]

-- | A deck list's 'entries', each with the section the headers before it
-- name: the main deck before any.
sectioned :: Text.Text -> [(DeckList.Section, Natural, CardName.CardName)]
sectioned contents =
  let step (section, kept) line = case DeckList.sectionNamed (Text.strip line) of
        Just next -> (next, kept)
        Nothing -> (section, kept <> [(section, n, card) | (n, card) <- entries line])
   in snd (List.foldl' step (DeckList.Main, []) (Text.lines contents))

-- | A deck list's lines that name a card ("4 Forest", "1 Shock (M19) 156"),
-- each as its count and the card; other lines skipped.
entries :: Text.Text -> [(Natural, CardName.CardName)]
entries contents =
  let entry line = do
        let (count, rest) = Text.span Char.isDigit (Text.strip line)
        n <- Read.readMaybe (Text.unpack count)
        pure (n, CardName.MkCardName (Text.strip (fst (Text.breakOn (Text.pack " (") rest))))
   in Maybe.mapMaybe entry (Text.lines contents)

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
update event typed lobby
  | paused lobby && abandons event lobby = lobby {abandoning = Just event}
  | otherwise = settled lobby (updated event typed lobby)

-- | Whether this event, done, abandons a game in progress: starting a game,
-- or inviting a player, whose link needs the data channels the game set
-- aside holds (Parun.Browser.aside).
abandons :: Event -> Lobby -> Bool
abandons event lobby = case event of
  MkStart -> Either.isRight (startable lobby)
  MkAdd MkInvited -> length (theirs lobby) < mostOthers
  _ -> False

-- | The lobby once the game in progress is abandoned, and the event asked
-- about, to do next; nothing where none was asked about.
abandoned :: Lobby -> Maybe (Lobby, Event)
abandoned lobby = (,) lobby {paused = False, abandoning = Nothing} <$> abandoning lobby

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
  MkBrowse e -> browsed (DeckBrowser.update e typed) lobby
  MkRandom -> lobby
  MkAdd occupant
    | i < mostOthers ->
        let added = lobby {theirs = theirs lobby <> [Nothing], choosing = MkTheirs i}
         in case occupant of
              MkBot -> added
              MkInvited -> added {choosing = choosing lobby, invites = Map.insert i (newInvite (nextLink lobby)) (invites lobby), nextLink = nextLink lobby + 1}
              MkHere sitting -> added {locals = Map.insert i sitting (locals lobby)}
    | otherwise -> lobby
    where
      i = length (theirs lobby)
  MkDrop i
    | removable lobby i ->
        let moved j = if j > i then j - 1 else j
         in lobby
              { theirs = [deck | (j, deck) <- zip [0 ..] (theirs lobby), j /= i],
                choosing = case choosing lobby of
                  MkTheirs j | j == i -> MkMine | otherwise -> MkTheirs (moved j)
                  MkMine -> MkMine,
                invites = Map.mapKeys moved (Map.delete i (invites lobby)),
                locals = Map.mapKeys moved (Map.delete i (locals lobby)),
                bots = Map.mapKeys moved (Map.delete i (bots lobby))
              }
    | otherwise -> lobby
  MkStart -> lobby
  MkOpenBuilder -> lobby
  MkEdit _ -> lobby
  MkReplyCode i -> lobby {invites = Map.adjust (\invite -> invite {replyCode = typed}) i (invites lobby)}
  MkConnect _ -> lobby
  MkReady -> lobby {ready = not (ready lobby)}
  MkDifficulty i -> maybe lobby (\d -> styled i (\(_, p) -> (d, p))) (Opponent.named Opponent.difficultyName (Text.unpack typed))
  MkPersona i -> maybe lobby (\p -> styled i (\(d, _) -> (d, p))) (Opponent.named Opponent.personaName (Text.unpack typed))
  MkAbandon -> lobby
  MkResume -> lobby
  MkCancel -> lobby {abandoning = Nothing}
  MkAnte -> optioned (\o -> o {ante = not (ante o)})
  MkPlanechase -> optioned (\o -> o {planechase = not (planechase o)})
  MkSinglePlanarDeck -> optioned (\o -> o {singlePlanarDeck = not (singlePlanarDeck o)})
  MkAttackOption -> maybe lobby (\a -> optioned (\o -> o {attackOption = a})) (valued attackOptions)
  MkRangeOfInfluence -> maybe lobby (\r -> optioned (\o -> o {rangeOfInfluence = r})) (valued ranges)
  MkClock -> maybe lobby (\c -> optioned (\o -> o {clock = c})) (valued Clock.choices)
  where
    optioned f = lobby {options = f (options lobby)}
    valued :: [(a, (String, String))] -> Maybe a
    valued among = Maybe.listToMaybe [option | (option, (value, _)) <- among, value == Text.unpack typed]
    styled i f
      | removable lobby i = lobby {bots = Map.insert i (f (botAt i (bots lobby))) (bots lobby)}
      | otherwise = lobby

-- | Whether the seat at this place in 'theirs' may be unseated: it is
-- seated.
removable :: Lobby -> Int -> Bool
removable lobby i = i >= 0 && i < length (theirs lobby)

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

-- | The lobby after a change, every ready flag and Start's refusal cleared
-- where the match-up changed: a deck, a seat, an invite or an option.
settled :: Lobby -> Lobby -> Lobby
settled before after
  | matchupOf before == matchupOf after = after
  | otherwise = (unready after) {refusal = Nothing}
  where
    matchupOf l = (mine l, theirs l, locals l, options l, fmap (\invite -> (connected invite, fmap fst (guestDeck invite), fmap snd (guestDeck invite))) (invites l))

-- | The lobby with every ready flag cleared.
unready :: Lobby -> Lobby
unready lobby = lobby {ready = False, invites = fmap (\invite -> invite {guestReady = False}) (invites lobby)}

-- | The lobby a Rematch of a game with players invited returns to: the
-- decks played, your own first, each bot's slot in `invited` held by its
-- player, still connected over the link the game used (its number there),
-- each in `here` played on this screen as it was, and nobody ready.
rematch :: NonEmpty.NonEmpty (Text.Text, Text.Text) -> Map.Map Int Int -> Map.Map Int Link.Sitting -> Lobby
rematch (yours NonEmpty.:| rest) invited here =
  new
    { mine = Just (fst yours),
      theirs = [if Map.member i invited then Nothing else Just name | (i, (name, _)) <- zip [0 ..] rest],
      invites = Map.fromList [(i, (newInvite n) {connected = True, guestDeck = Just seat}) | (i, seat) <- zip [0 ..] rest, Just n <- [Map.lookup i invited]],
      nextLink = foldr (max . (+ 1)) 0 invited,
      locals = Map.withoutKeys here (Map.keysSet invited),
      -- Your deck's and each bot's name, as their lists give them, until
      -- the decks are read again.
      browser = (browser new) {DeckBrowser.titles = foldMap DeckBrowser.titleIn (yours : [seat | (i, seat) <- zip [0 ..] rest, Map.notMember i invited])}
    }

-- | Whether a game with the players invited starts: there is one, your
-- deck is chosen, and every player is connected, has a deck and is ready.
starts :: Lobby -> Bool
starts lobby =
  not (Map.null (invites lobby))
    && ready lobby
    && Maybe.isJust (mine lobby)
    && guestsReady lobby

-- | Whether every player invited is connected, with a deck, and ready, so
-- that you being ready starts the game.
guestsReady :: Lobby -> Bool
guestsReady = all (\invite -> connected invite && Maybe.isJust (guestDeck invite) && guestReady invite) . invites

-- | The match-up as the player invited to this slot is shown it, each
-- deck coloured by its name (`named`) or, a guest's, by its list (`listed`).
placesFor :: (Text.Text -> Set.Set Color.Color) -> (Text.Text -> Set.Set Color.Color) -> Int -> Lobby -> [Link.Place]
placesFor named listed i lobby =
  Link.MkPlace (Text.pack "Player 1") (maybe Link.unchosen (DeckBrowser.label (DeckBrowser.titles (browser lobby))) (mine lobby)) (Just (ready lobby)) False (foldMap named (mine lobby))
    : [ case Map.lookup j (invites lobby) of
          Just invite -> Link.MkPlace (if j == i then Text.pack "You" else Text.pack (seatName (MkTheirs j))) (maybe Link.unchosen (\deck -> DeckBrowser.label (DeckBrowser.titleIn deck) (fst deck)) (guestDeck invite)) (Just (guestReady invite)) False (foldMap (listed . snd) (guestDeck invite))
          Nothing -> Link.MkPlace (Text.pack (seatName (MkTheirs j))) (maybe (Text.pack "Random deck") (DeckBrowser.label (DeckBrowser.titles (browser lobby))) held) Nothing (Map.notMember j (locals lobby)) (foldMap named held)
      | (j, held) <- zip [0 ..] (theirs lobby)
      ]

-- | The game's options as an invited player is told them, each a label and
-- its value as 'optionsView' shows them.
told :: Lobby -> [(Text.Text, Text.Text)]
told lobby =
  [(Text.pack "Play for ante", yesNo (ante chosen')), (Text.pack "Planechase", yesNo (planechase chosen'))]
    <> [(Text.pack "Single planar deck", yesNo (singlePlanarDeck chosen')) | planechase chosen']
    <> [(Text.pack "Clock", labelIn Clock.choices (clock chosen'))]
    <> ( if multiplayer (seatCount lobby)
           then [(Text.pack "Attacks", labelIn attackOptions (attackOption chosen')), (Text.pack "Range of influence", labelIn ranges (rangeOfInfluence chosen'))]
           else []
       )
  where
    chosen' = options lobby
    yesNo on = Text.pack (if on then "Yes" else "No")
    labelIn :: (Eq a) => [(a, (String, String))] -> a -> Text.Text
    labelIn choices option = Text.pack (maybe "" snd (List.lookup option choices))

-- | Your deck, to start a game with; or why it may not: the problems a
-- check found with it, or none while there is no deck.
startable :: Lobby -> Either [Text.Text] Text.Text
startable lobby = case mine lobby of
  Nothing -> Left []
  Just name -> maybe (Right name) Left (Map.lookup name (DeckBrowser.illegal (browser lobby)))

-- | The deck chosen for a slot, if any.
chosen :: Lobby -> Slot -> Maybe Text.Text
chosen lobby slot = case slot of
  MkMine -> mine lobby
  MkTheirs i -> Monad.join (Maybe.listToMaybe (drop i (theirs lobby)))

-- | The decks the seat being chosen for may play: every deck for yours, its
-- 'opponents' for a bot's.
offered :: Lobby -> Seq.Seq Text.Text
offered lobby = if choosing lobby == MkMine then decks lobby else opponents lobby

-- | The decks shown to the seat being chosen for (DeckBrowser.shown).
shown :: Lobby -> [Text.Text]
shown lobby = DeckBrowser.shown (offered lobby) (browser lobby)

-- | A seat's name, as the game will call it (Parun.Play.Wording.player):
-- "You", else "Player 2" by its seat, whoever plays it.
seatName :: Slot -> String
seatName slot = case slot of
  MkMine -> "You"
  MkTheirs i -> "Player " <> show (i + 2)

-- | What stops Start, or Ready with players invited: the decks unread, or
-- your deck unchosen or breaking its format. A bot always has a deck to
-- draw, since your deck is among its 'opponents'.
blocking :: Lobby -> Maybe String
blocking lobby
  | DeckBrowser.reading (browser lobby) = Just "Reading the decks\x2026"
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
      | not (connected invite) -> Just (seatName (MkTheirs i) <> " is still to connect.")
      | Maybe.isNothing (guestDeck invite) -> Just (seatName (MkTheirs i) <> " is choosing a deck.")
      | otherwise -> Just (seatName (MkTheirs i) <> " is not ready yet.")
    [] -> Nothing

-- | Whether Start, or Ready with players invited, may be pressed.
pressable :: Lobby -> Bool
pressable = Maybe.isNothing . blocking

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

-- | The seats: yours, then each bot's with how it plays, invited player's,
-- player's here or one you play too; the way to unseat each that is
-- 'removable'; a seat to add, by who fills it, while there is room; the
-- game's options; and Start at the foot, saying what it waits on.
seatsView :: Lobby -> Html.Html Event
seatsView lobby =
  Element.el "section" ["seats"] [Element.attr "aria-label" (Text.pack "Seats")] $
    [ Element.el "h1" [] [] [Element.string "New game"],
      Element.el "span" ["eyebrow"] [] [Element.string "At the table"],
      seat MkMine (Element.string "Y") (mine lobby) [] []
    ]
      <> concat
        [ Element.el "span" ["eyebrow", "versus"] [] [Element.string "versus"]
            : case Map.lookup i (invites lobby) of
              Just invite -> [inviteView i invite (remove i)]
              Nothing -> case Map.lookup i (locals lobby) of
                Just sitting -> [seat (MkTheirs i) (Element.string (if sitting == Link.MkYours then "Y" else "P")) held [] (remove i)]
                Nothing -> [seat (MkTheirs i) (Icon.icon [] Icon.MkBot) held [BotSettings.botView (show i) (MkDifficulty i) (MkPersona i) (botAt i (bots lobby))] (remove i)]
        | (i, held) <- zip [0 ..] others
        ]
      <> [ Element.el
             "div"
             ["seat-tools"]
             [Element.attr "role" (Text.pack "group"), Element.attr "aria-labelledby" (Text.pack "add-seat")]
             [ Element.el "span" ["eyebrow"] [Element.attr "id" (Text.pack "add-seat")] [Element.string (if null others then "Add a seat, or play alone" else "Add a seat")],
               Element.el
                 "div"
                 ["seat-choices"]
                 []
                 [ Element.button ["add-bot"] (MkAdd MkBot) [Element.string "Bot"],
                   Element.button ["add-here"] (MkAdd (MkHere Link.MkHandedOver)) [Element.string "Player here"],
                   Element.button ["add-invite"] (MkAdd MkInvited) [Element.string "Invite a player"],
                   Element.button ["add-yours"] (MkAdd (MkHere Link.MkYours)) [Element.string "Yourself"]
                 ]
             ]
         | length others < mostOthers
         ]
      <> [optionsView lobby, startView lobby]
  where
    others = theirs lobby
    remove i = [Element.el "button" ["quiet", "drop-seat"] [Element.attr "type" (Text.pack "button"), Element.on "click" (MkDrop i), Element.attr "aria-label" (Text.pack ("Remove " <> seatName (MkTheirs i)))] [Element.string "Remove"] | removable lobby i]
    seat which avatar held below extra =
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
                  ( [Element.el "span" ["avatar"] [] [avatar], Element.el "strong" ["seat-name"] [] [Element.string (seatName which)]]
                      <> [Element.el "span" ["chip", "bot"] [] [Element.string "Bot"] | MkTheirs i <- [which], Map.notMember i (locals lobby)]
                      <> [Element.el "span" ["chip", "choosing"] [] [Element.string "Choosing a deck"] | on, Maybe.isNothing held]
                  ),
                Element.el "span" ["seat-deck"] [] $ case held of
                  Just name -> [DeckBrowser.pips (Map.findWithDefault Set.empty name (DeckBrowser.palette (browser lobby))), Element.el "span" ["seat-deck-name"] [] [Element.text (snd (DeckBrowser.describe (DeckBrowser.titles (browser lobby)) name))]]
                  Nothing -> [Element.el "span" ["seat-deck-name"] [] [Element.string (if which == MkMine then "No deck yet" else "Random deck")]],
                Element.el "span" ["seat-note", "muted"] [] [Element.string (maybe (if which == MkMine then "Choose one to start" else "Drawn when the game starts") (DeckBrowser.counted (browser lobby)) held)]
              ]
              : below
                <> [Element.el "div" ["seat-actions"] [] extra | not (null extra)]

-- | The game's options: ante, Planechase and, while it is on, the single
-- planar deck, each seat's clock, and at three seats or more CR 806.2's
-- attack option and range of influence.
optionsView :: Lobby -> Html.Html Event
optionsView lobby =
  Element.el "fieldset" ["game-options"] [] $
    [ Element.el "legend" ["eyebrow"] [] [Element.string "Game options"],
      toggle "option-ante" MkAnte (ante chosen') "Play for ante" "Each player antes a random card from their library; the winner owns them all.",
      toggle "option-planechase" MkPlanechase (planechase chosen') "Planechase" "Each seat plays its list's planar deck, or one dealt from the planes and phenomena."
    ]
      <> [toggle "option-single-planar-deck" MkSinglePlanarDeck (singlePlanarDeck chosen') "Single planar deck" "Every seat's planar deck is shuffled into one." | planechase chosen']
      <> [menu "option-clock" MkClock "Clock" [(value, label, option == clock chosen') | (option, (value, label)) <- Clock.choices]]
      <> ( if multiplayer (seatCount lobby)
             then
               [ menu "option-attack" MkAttackOption "Attacks" [(value, label, option == attackOption chosen') | (option, (value, label)) <- attackOptions],
                 menu "option-range" MkRangeOfInfluence "Range of influence" [(value, label, option == rangeOfInfluence chosen') | (option, (value, label)) <- ranges]
               ]
             else []
         )
  where
    chosen' = options lobby
    toggle :: String -> Event -> Bool -> String -> String -> Html.Html Event
    toggle field event on label note =
      Element.el
        "div"
        ["game-option"]
        []
        [ Element.el "input" [] ([Element.attr "id" (Text.pack field), Element.attr "type" (Text.pack "checkbox"), Element.attr "aria-describedby" (Text.pack (field <> "-note")), Element.on "change" event] <> [Element.attr "checked" Text.empty | on]) [],
          Element.el "label" [] [Element.attr "for" (Text.pack field)] [Element.string label],
          Element.el "p" ["muted"] [Element.attr "id" (Text.pack (field <> "-note"))] [Element.string note]
        ]
    menu :: String -> Event -> String -> [(String, String, Bool)] -> Html.Html Event
    menu field event label choices =
      Element.el
        "div"
        ["game-option"]
        []
        [ Element.el "label" [] [Element.attr "for" (Text.pack field)] [Element.string label],
          Element.el
            "select"
            ["deck-menu"]
            [Element.attr "id" (Text.pack field), Element.on "change" event]
            [Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | on]) [Element.string text] | (value, text, on) <- choices]
        ]

-- | Start, or Ready with players invited, and what it waits on; else,
-- asked about, whether to abandon the game in progress.
startView :: Lobby -> Html.Html Event
startView lobby = case abandoning lobby of
  Just asked ->
    Element.el
      "section"
      ["panel", "confirm"]
      [Element.attr "role" (Text.pack "alertdialog"), Element.attr "aria-label" (Text.pack "Abandon the game in progress?")]
      [ Element.el "p" [] [] [Element.string "Starting a new game abandons the one in progress, which Replays won't keep."],
        Element.el
          "div"
          ["choices"]
          []
          [ Element.button ["primary", "resume"] MkResume [Element.string "Resume"],
            Element.button [] MkAbandon [Element.string (if asked == MkStart then "Abandon and play" else "Abandon and invite")],
            Element.button [] MkCancel [Element.string "Cancel"]
          ]
      ]
  Nothing -> starting lobby

-- | Start, or Ready with players invited, and what it waits on.
starting :: Lobby -> Html.Html Event
starting lobby =
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
            [Element.string (if ready lobby then "Ready \x2713 \x00b7 waiting for every player" else if guestsReady lobby then "Start game" else "Ready")],
      Element.el "p" ["start-note", "muted"] [Element.attr "id" (Text.pack "start-note"), Element.attr "aria-live" (Text.pack "polite")] [Element.string (Maybe.fromMaybe (if null (theirs lobby) then "Just you: a goldfish game. Ready to start." else "Every seat has a deck. Ready to start.") (waiting lobby))]
    ]
      <> [Element.el "ul" ["problems", "refusal"] [] [Element.el "li" [] [] [Element.text p] | p <- problems] | Left problems@(_ : _) <- [startable lobby]]
      <> [Element.el "p" ["problem", "planar-refusal"] [Element.attr "role" (Text.pack "alert")] [Element.text why] | why <- Maybe.maybeToList (refusal lobby)]

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
          [ Element.el "h2" [] [] [Element.string ("Deck for " <> seatName (choosing lobby))],
            Element.button ["open-import"] (MkBrowse DeckBrowser.MkOpenImport) [Element.string "Import a deck"],
            Element.button ["open-builder"] MkOpenBuilder [Element.string "Build a deck"]
          ]
      )
    ]
      <> foldMap (pure . (,) (Text.pack "import") . fmap MkBrowse . DeckBrowser.importView) (DeckBrowser.importing browsing)
      <> [ (Text.pack "shelves", fmap MkBrowse (DeckBrowser.shelvesView browsing)),
           (Text.pack "filters", DeckBrowser.filtersView MkBrowse [Element.button ["random"] MkRandom [Element.string "Surprise me"]] browsing),
           (Text.pack "decks", DeckBrowser.resultsView MkPick ((== held) . Just) [Element.el "p" ["empty"] [] [Element.string "No saved decks yet: build or import one."]] (offered lobby) browsing)
         ]
  where
    browsing = browser lobby
    held = chosen lobby (choosing lobby)

-- | The deck of the seat being chosen for: its detail (DeckBrowser.detailBody),
-- and the ways to edit it, and a saved one's to copy it out and delete it.
detailView :: Lobby -> Html.Html Event
detailView lobby =
  Element.el "aside" ["detail"] [Element.attr "aria-label" (Text.pack "Deck chosen")] $
    Element.el "span" ["eyebrow"] [] [Element.string (if choosing lobby == MkMine then "Your deck" else seatName (choosing lobby) <> "'s deck")]
      : case chosen lobby (choosing lobby) of
        Nothing ->
          [ Element.el "h2" [] [] [Element.string (if choosing lobby == MkMine then "No deck yet" else "Random deck")],
            Element.el "p" ["muted"] [] [Element.string (if choosing lobby == MkMine then "Pick one of the decks to see it here." else "A deck sharing your deck's format, drawn when the game starts. Pick one to choose it.")]
          ]
        Just name ->
          DeckBrowser.detailBody (browser lobby) name
            <> [ Element.el "div" ["saved-tools"] [] $
                   if DeckBrowser.isSaved name
                     then [Element.button ["edit"] (MkEdit name) [Element.string "Edit"], Element.button ["export-deck"] (MkBrowse (DeckBrowser.MkExport name)) [Element.string "Export"], Element.button ["delete"] (MkBrowse (DeckBrowser.MkDelete name)) [Element.string "Delete"]]
                     else [Element.button ["edit"] (MkEdit name) [Element.string "Edit a copy"]]
               ]
            <> fmap (fmap MkBrowse) (DeckBrowser.exportedView (browser lobby) name)

-- | A slot a player from another browser takes: the link to send them, the
-- box their reply code goes in, and once connected their deck and whether
-- they are ready; the warning that the host sees every card; and `extra`,
-- the way to unseat it where there is one.
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
              ( [Element.el "span" ["avatar"] [] [Element.string "P"], Element.el "strong" ["seat-name"] [] [Element.string (seatName (MkTheirs i))]]
                  <> [Element.el "span" ["chip"] [] [Element.string (if guestReady invite then "Ready \x2713" else "Not ready")] | connected invite]
              ),
            Element.el "span" ["seat-deck"] [] [Element.el "span" ["seat-deck-name"] [] [Element.text (maybe (Text.pack (if connected invite then "Choosing a deck\x2026" else "Not connected")) (\deck -> snd (DeckBrowser.describe (DeckBrowser.titleIn deck) (fst deck))) (guestDeck invite))]],
            Element.el "span" ["seat-note", "muted"] [] [Element.string (if connected invite then "Chooses their own deck" else "Invited from another browser")]
          ]
      ]
      <> ( if connected invite
             then []
             else case link invite of
               Nothing -> [Element.el "p" ["invite-step"] [] [Element.string "Making the link\x2026"]]
               Just url ->
                 [ Element.el "label" [] [Element.attr "for" (field "invite-link")] [Element.string "1. Send them this link"],
                   Element.copyable ("invite-link-" <> show i) url,
                   Element.el "label" [] [Element.attr "for" (field "invite-reply")] [Element.string "2. Paste the code they send back"],
                   Element.el "input" ["invite-reply"] [Element.attr "id" (field "invite-reply"), Element.attr "type" (Text.pack "text"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "spellcheck" (Text.pack "false"), Element.on "input" (MkReplyCode i)] [],
                   Element.el "button" ["connect"] ([Element.attr "type" (Text.pack "button"), Element.on "click" (MkConnect i)] <> [Element.attr "disabled" Text.empty | Text.null (Text.strip (replyCode invite))]) [Element.string "Connect"]
                 ]
         )
      <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text p] | p <- Maybe.maybeToList (inviteProblem invite)]
      <> [Element.el "p" ["invite-warning"] [] [Element.string "Your browser runs the game, so you could see their hidden cards: invite only friends."]]
      <> [Element.el "div" ["seat-actions"] [] extra | not (null extra)]
  where
    field name = Text.pack (name <> "-" <> show i)
