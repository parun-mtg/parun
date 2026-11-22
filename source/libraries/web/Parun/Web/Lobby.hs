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
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Hybrid as Hybrid
import qualified Parun.Types.HybridPhyrexian as HybridPhyrexian
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.RuleFamily as RuleFamily
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
    -- | Each bot's deck, in seat order, once chosen; a random one otherwise.
    -- There is at least one bot and at most 'mostBots'.
    theirs :: [Maybe Text.Text],
    -- | The seat a tapped deck goes to, which only a tap on a seat changes.
    choosing :: Slot,
    -- | Each bot's slot switched to a player invited from another browser,
    -- by its place in 'theirs' (Parun.Web.Link).
    invites :: Map.Map Int Invite,
    -- | Whether you are ready to start a game with the players invited.
    ready :: Bool,
    -- | How strongly every bot plays (Parun.Play.Opponent).
    difficulty :: Difficulty.Difficulty,
    -- | What every bot values in a position (Parun.Play.Opponent).
    persona :: Persona.Persona
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
  | -- | Something done in the deck browser.
    MkBrowse DeckBrowser.Event
  | -- | A random deck of those shown, for the slot being chosen; the shell
    -- picks it.
    MkRandom
  | -- | Seats another bot, with a random deck, up to 'mostBots'.
    MkAddBot
  | -- | Unseats the last bot, while there are two or more.
    MkDropBot
  | -- | Starts the game.
    MkStart
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
  | -- | Sets every bot's difficulty to the one whose name is chosen.
    MkDifficulty
  | -- | Sets every bot's persona to the one whose name is chosen.
    MkPersona
  deriving (Eq, Show)

-- | A lobby whose decks are still to read, nothing chosen yet, every bot
-- playing as `parun play`'s does unless told otherwise.
new :: Lobby
new =
  MkLobby
    { browser = DeckBrowser.new,
      mine = Nothing,
      theirs = [Nothing],
      choosing = MkMine,
      invites = Map.empty,
      ready = False,
      difficulty = Difficulty.MkHard,
      persona = Persona.MkBalanced
    }

-- | A new lobby with this deck yours, on the shelf it is on.
holding :: Text.Text -> Lobby
holding name = browsed (\b -> b {DeckBrowser.shelf = DeckBrowser.shelfOf name}) new {mine = Just name}

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

-- | The most bots a game seats: a table of four.
mostBots :: Int
mostBots = 3

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
  MkBrowse e -> browsed (DeckBrowser.update e typed) lobby
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
  MkDifficulty -> maybe lobby (\d -> lobby {difficulty = d}) (Opponent.named Opponent.difficultyName (Text.unpack typed))
  MkPersona -> maybe lobby (\p -> lobby {persona = p}) (Opponent.named Opponent.personaName (Text.unpack typed))

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
      invites = Map.fromList [(i, newInvite {connected = True, guestDeck = Just seat}) | (i, seat) <- zip [0 ..] rest, Set.member i invited],
      -- Your deck's and each bot's name, as their lists give them, until
      -- the decks are read again.
      browser = (browser new) {DeckBrowser.titles = foldMap DeckBrowser.titleIn (yours : [seat | (i, seat) <- zip [0 ..] rest, Set.notMember i invited])}
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
  Link.MkPlace (Text.pack "Host") (maybe (Text.pack "Choosing a deck") (DeckBrowser.label (DeckBrowser.titles (browser lobby))) (mine lobby)) (Just (ready lobby))
    : [ case Map.lookup j (invites lobby) of
          Just invite -> Link.MkPlace (if j == i then Text.pack "You" else Text.pack ("Player " <> show (j + 2))) (maybe (Text.pack "Choosing a deck") (\deck -> DeckBrowser.label (DeckBrowser.titleIn deck) (fst deck)) (guestDeck invite)) (Just (guestReady invite))
          Nothing -> Link.MkPlace (Text.pack (if length (theirs lobby) == 1 then "Bot" else "Bot " <> show (j + 1))) (maybe (Text.pack "Random deck") (DeckBrowser.label (DeckBrowser.titles (browser lobby))) held) Nothing
      | (j, held) <- zip [0 ..] (theirs lobby)
      ]

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
      | not (connected invite) -> Just (seatName lobby (MkTheirs i) <> " is still to connect.")
      | Maybe.isNothing (guestDeck invite) -> Just (seatName lobby (MkTheirs i) <> " is choosing a deck.")
      | otherwise -> Just (seatName lobby (MkTheirs i) <> " is not ready yet.")
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
             )
         ]
      <> [botView lobby | length bots > Map.size (invites lobby)]
      <> [startView lobby]
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
                      <> [Element.el "span" ["chip", "choosing"] [] [Element.string "Choosing a deck"] | on, Maybe.isNothing held]
                  ),
                Element.el "span" ["seat-deck"] [] $ case held of
                  Just name -> [DeckBrowser.pips (Map.findWithDefault Set.empty name (DeckBrowser.palette (browser lobby))), Element.el "span" ["seat-deck-name"] [] [Element.text (snd (DeckBrowser.describe (DeckBrowser.titles (browser lobby)) name))]]
                  Nothing -> [Element.el "span" ["seat-deck-name"] [] [Element.string (if which == MkMine then "No deck yet" else "Random deck")]],
                Element.el "span" ["seat-note", "muted"] [] [Element.string (maybe (if which == MkMine then "Choose one to start" else "Drawn when the game starts") (DeckBrowser.counted (browser lobby)) held)]
              ]
              : [Element.el "div" ["seat-actions"] [] extra | not (null extra)]

-- | How every bot plays: its difficulty and its persona, each a choice of
-- those named (Parun.Play.Opponent).
botView :: Lobby -> Html.Html Event
botView lobby =
  Element.el
    "div"
    ["bot-settings"]
    []
    [ Element.el "label" [] [Element.attr "for" (Text.pack "bot-difficulty")] [Element.string "Bot level"],
      choice "bot-difficulty" MkDifficulty Opponent.difficultyName (difficulty lobby),
      Element.el "label" [] [Element.attr "for" (Text.pack "bot-persona")] [Element.string "Bot style"],
      choice "bot-persona" MkPersona Opponent.personaName (persona lobby)
    ]
  where
    choice :: (Bounded a, Enum a, Eq a) => String -> Event -> (a -> (String, String)) -> a -> Html.Html Event
    choice field event nameOf current =
      Element.el
        "select"
        [field]
        [Element.attr "id" (Text.pack field), Element.on "change" event]
        [ Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | option == current]) [Element.string label]
        | option <- [minBound .. maxBound],
          let (value, label) = nameOf option
        ]

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
            Element.button ["open-import"] (MkBrowse DeckBrowser.MkOpenImport) [Element.string "Import a deck"],
            Element.button ["open-builder"] MkOpenBuilder [Element.string "Build a deck"]
          ]
      )
    ]
      <> foldMap (pure . (,) (Text.pack "import") . fmap MkBrowse . DeckBrowser.importView) (DeckBrowser.importing browsing)
      <> [ (Text.pack "shelves", fmap MkBrowse (DeckBrowser.shelvesView [DeckBrowser.MkPrecons, DeckBrowser.MkSaved] browsing)),
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
    Element.el "span" ["eyebrow"] [] [Element.string (if choosing lobby == MkMine then "Your deck" else seatName lobby (choosing lobby) <> "'s deck")]
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
