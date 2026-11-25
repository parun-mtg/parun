-- | A seat played from another browser (#9185): the messages the hosting page
-- and a guest's page send each other over their data channel, each a line of
-- JSON tagged with its type and the build that sent it, and both ends of a
-- game played over them. The host runs the engine; the guest only answers
-- the requests its seat is sent.
module Parun.Web.Link where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Color as Codec.Color
import qualified Parun.Codec.GameSettings as Codec.GameSettings
import qualified Parun.Codec.PlayerId as Codec.PlayerId
import qualified Parun.Codec.PlayerView as Codec.PlayerView
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Key as Key.Type
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Route as Route

-- | What one page tells the other.
data Message
  = -- | The guest's deck, by its name and its list's text; sent again on a
    -- change of deck.
    MkHello Text.Text Text.Text
  | -- | The match-up as it stands and the game's options, each a label and
    -- its value as the host's lobby shows them (Lobby.told), sent to the
    -- guest at every change.
    MkMatchup [Place] [(Text.Text, Text.Text)]
  | -- | The guest is ready to start, or no longer.
    MkReady Bool
  | -- | The game starts, or goes on after a rejoin, the guest playing this
    -- seat, which has seen this so far (seeing): a page that reloaded starts
    -- its log from it.
    MkStart PlayerId.PlayerId Ending.Seen
  | -- | A request the guest's seat answers, and each timed seat's time left
    -- (Clock.banks).
    MkAsk Request.Request Banks
  | -- | The seat whose decision the game now waits on, another's than the
    -- guest's, the game as the guest's seat sees it then, at this depth
    -- (Request.depth), less whose priority it is (telling), and each timed
    -- seat's time left.
    MkDeciding PlayerId.PlayerId PlayerView.PlayerView Natural Banks
  | -- | The guest's reply to the last request.
    MkAnswer R.Reply
  | -- | The game ended: the guest's ending, its log and life trail built
    -- from what the host sent its seat (seeing), which a page that reloaded
    -- lost.
    MkEnd Ending.Ending
  deriving (Eq, Show)

-- | Each timed seat's time left, in seconds (Clock.banks).
type Banks = Map.Map PlayerId.PlayerId Double

-- | Time banks on the wire, in whole milliseconds.
banksCodec :: Codec.Codec Banks
banksCodec = Common.keyedList (Common.tuple Codec.PlayerId.codec millis)
  where
    millis :: Codec.Codec Double
    millis = Codec.MkCodec {Codec.encode = Codec.encode Common.natural . round . (* 1000) . max 0, Codec.decode = fmap ((/ 1000) . Natural.toDouble) . Codec.decode Common.natural, Codec.schema = Codec.schema Common.natural}

-- | A seat of the match-up as the guest is shown it.
data Place = MkPlace
  { -- | Its name, as the game calls it to the guest: "Player 1", "You".
    who :: Text.Text,
    -- | Its deck as the match-up names it (DeckBrowser.label), or what stands for
    -- one not chosen yet.
    deck :: Text.Text,
    -- | Whether the person playing it is ready; Nothing for a bot or a
    -- seat played on the host's screen.
    ready :: Maybe Bool,
    -- | Whether a bot plays it.
    bot :: Bool,
    -- | Its deck's colours, as the host reads them, for the guest's
    -- history; none while it is not chosen.
    colours :: Set.Set Color.Color
  }
  deriving (Eq, Show)

-- | What a place's deck says while its player is still to choose one.
unchosen :: Text.Text
unchosen = Text.pack "Choosing a deck"

-- | A message as a line, stamped with the build that sends it.
encode :: Text.Text -> Message -> Text.Text
encode build message =
  let (tag, fields) = case message of
        MkHello name contents -> ("hello", [Value.pair "name" (Value.text name), Value.pair "list" (Value.text contents)])
        MkMatchup places options -> ("matchup", [Value.pair "seats" (Value.array (fmap place places)), Value.pair "options" (Codec.encode (Common.list (Common.tuple Common.text Common.text)) options)])
        MkReady on -> ("ready", [Value.pair "ready" (Value.boolean on)])
        MkStart seat seen' -> ("start", [Value.pair "seat" (Value.integer (toInteger (PlayerId.unwrap seat))), Value.pair "seen" (seenValue seen')])
        MkAsk request clocks -> ("ask", [Value.pair "request" (Codec.encode Codec.Request.codec request), Value.pair "clocks" (Codec.encode banksCodec clocks)])
        MkDeciding seat shown depth clocks -> ("deciding", [Value.pair "seat" (Value.integer (toInteger (PlayerId.unwrap seat))), Value.pair "view" (Codec.encode Codec.PlayerView.codec shown), Value.pair "depth" (Codec.encode Common.natural depth), Value.pair "clocks" (Codec.encode banksCodec clocks)])
        MkAnswer reply -> ("answer", [Value.pair "reply" (Codec.Reply.toValue reply)])
        MkEnd ending' ->
          ( "end",
            [ Value.pair "verdict" (Codec.encode Ending.verdictCodec (Ending.verdict ending')),
              Value.pair "said" (Value.array (fmap Value.text (Ending.said ending'))),
              Value.pair "scenario" (Value.text (Ending.scenario ending')),
              Value.pair "log" (Value.array (fmap entryValue (Foldable.toList (Ending.witnessed ending')))),
              Value.pair "seat" (Codec.encode Codec.PlayerId.codec (Ending.viewer ending')),
              Value.pair "shared" (Value.boolean (Ending.shared ending')),
              Value.pair "trail" (Codec.encode trail (Ending.trail ending'))
            ]
          )
      place p = Value.object [Value.pair "who" (Value.text (who p)), Value.pair "deck" (Value.text (deck p)), Value.pair "ready" (maybe Value.null Value.boolean (ready p)), Value.pair "bot" (Value.boolean (bot p)), Value.pair "colours" (Codec.encode (Common.set Codec.Color.codec) (colours p))]
   in Common.render (Value.object (Value.pair "type" (Value.string tag) : Value.pair "build" (Value.text build) : fields))

-- | A line's message, or why it does not read: one from another build is
-- refused, since its requests and replies may not mean the same.
decode :: Text.Text -> Text.Text -> Either Text.Text Message
decode build line = do
  pairs <- Common.parse line >>= Common.asObject
  theirs <- Common.field "build" pairs >>= Common.asText
  Monad.unless (theirs == build) . Left $ Text.pack "the other page runs another build of Parun (" <> theirs <> Text.pack ", this one " <> build <> Text.pack "): reload both pages"
  tag <- Common.field "type" pairs >>= Common.asText
  let text key = Common.field key pairs >>= Common.asText
      clocks = Common.field "clocks" pairs >>= Codec.decode banksCodec
  case Text.unpack tag of
    "hello" -> MkHello <$> text "name" <*> text "list"
    "matchup" -> MkMatchup <$> (Common.field "seats" pairs >>= Common.asArray >>= traverse place) <*> (Common.field "options" pairs >>= Codec.decode (Common.list (Common.tuple Common.text Common.text)))
    "ready" -> MkReady <$> (Common.field "ready" pairs >>= Common.asBoolean)
    "start" -> MkStart <$> (Common.field "seat" pairs >>= Common.asInteger >>= natural) <*> (Common.field "seen" pairs >>= seenOf)
    "ask" -> MkAsk <$> (Common.field "request" pairs >>= Codec.decode Codec.Request.codec) <*> clocks
    "deciding" -> MkDeciding <$> (Common.field "seat" pairs >>= Common.asInteger >>= natural) <*> (Common.field "view" pairs >>= Codec.decode Codec.PlayerView.codec) <*> (Common.field "depth" pairs >>= Codec.decode Common.natural) <*> clocks
    "answer" -> MkAnswer <$> (Common.field "reply" pairs >>= Codec.Reply.fromValue)
    "end" ->
      fmap MkEnd $
        Ending.MkEnding
          <$> (Common.field "verdict" pairs >>= Codec.decode Ending.verdictCodec)
          <*> (Common.field "said" pairs >>= Common.asArray >>= traverse Common.asText)
          <*> text "scenario"
          <*> (Common.field "log" pairs >>= Common.asArray >>= fmap Seq.fromList . traverse entryOf)
          <*> (Common.field "seat" pairs >>= Codec.decode Codec.PlayerId.codec)
          <*> (Common.field "shared" pairs >>= Common.asBoolean)
          <*> (Common.field "trail" pairs >>= Codec.decode trail)
    other -> Left (Text.pack ("not a message: " <> other))
  where
    place value = do
      fields <- Common.asObject value
      MkPlace
        <$> (Common.field "who" fields >>= Common.asText)
        <*> (Common.field "deck" fields >>= Common.asText)
        <*> maybe (Right Nothing) (\v -> if v == Value.null then Right Nothing else Just <$> Common.asBoolean v) (Common.optionalField "ready" fields)
        <*> (Common.field "bot" fields >>= Common.asBoolean)
        <*> (Common.field "colours" fields >>= Codec.decode (Common.set Codec.Color.codec))
    natural :: Integer -> Either Text.Text PlayerId.PlayerId
    natural = maybe (Left (Text.pack "not a seat")) (Right . PlayerId.MkPlayerId) . Integer.toNatural

-- | A log entry on the wire.
entryValue :: Log.Entry -> Value.Value
entryValue e = case e of
  Log.MkPregame -> Value.object [Value.pair "pregame" (Value.boolean True)]
  Log.MkTurn n active -> Value.object [Value.pair "turn" (Value.integer (toInteger n)), Value.pair "active" (Codec.encode Codec.PlayerId.codec active)]
  Log.MkEvent weight said -> Value.object [Value.pair "event" (Value.text said), Value.pair "minor" (Value.boolean (weight == Log.MkMinor))]
  Log.MkMana pid sources -> Value.object [Value.pair "mana" (Codec.encode Codec.PlayerId.codec pid), Value.pair "from" (Value.array (fmap Value.text sources))]

-- | A log entry from the wire (entryValue).
entryOf :: Value.Value -> Either Text.Text Log.Entry
entryOf value = do
  fields <- Common.asObject value
  case (Common.optionalField "pregame" fields, Common.optionalField "turn" fields, Common.optionalField "mana" fields) of
    (Just _, _, _) -> Right Log.MkPregame
    (Nothing, Just n, _) -> Log.MkTurn <$> Codec.decode Common.natural n <*> (Common.field "active" fields >>= Codec.decode Codec.PlayerId.codec)
    (Nothing, Nothing, Just pid) -> Log.MkMana <$> Codec.decode Codec.PlayerId.codec pid <*> (Common.field "from" fields >>= Common.asArray >>= traverse Common.asText)
    (Nothing, Nothing, Nothing) -> flip Log.MkEvent <$> (Common.field "event" fields >>= Common.asText) <*> (Common.field "minor" fields >>= Common.asBoolean >>= \minor -> pure (if minor then Log.MkMinor else Log.MkMajor))

-- | What a page has seen (Ending.Seen) on the wire.
seenValue :: Ending.Seen -> Value.Value
seenValue seen' =
  Value.object
    [ Value.pair "marks" (Codec.encode marks (Map.toAscList (Ending.marks seen'))),
      Value.pair "logged" (Value.array [Value.array [Codec.encode Codec.PlayerId.codec seat, Value.array (fmap entryValue (Foldable.toList entries))] | (seat, entries) <- Map.toAscList (Ending.logged seen')]),
      Value.pair "lives" (Codec.encode trail (Foldable.toList (Ending.sampledLives seen')))
    ]

-- | What a page has seen from the wire (seenValue).
seenOf :: Value.Value -> Either Text.Text Ending.Seen
seenOf value = do
  fields <- Common.asObject value
  marks' <- Common.field "marks" fields >>= Codec.decode marks
  logged' <- Common.field "logged" fields >>= Common.asArray >>= traverse seatLog
  lives' <- Common.field "lives" fields >>= Codec.decode trail
  pure (Ending.MkSeen (Map.fromList marks') (Map.fromList logged') (Seq.fromList lives'))
  where
    seatLog pair = do
      items <- Common.asArray pair
      case items of
        [seat, entries] -> (,) <$> Codec.decode Codec.PlayerId.codec seat <*> (Common.asArray entries >>= fmap Seq.fromList . traverse entryOf)
        _ -> Left (Text.pack "not a seat's log")

-- | How far each seat has read each game's log (Ending.marks) on the wire.
marks :: Codec.Codec [((PlayerId.PlayerId, Natural), (Natural, Natural))]
marks = Common.list (Common.tuple (Common.tuple Codec.PlayerId.codec Common.natural) (Common.tuple Common.natural Common.natural))

-- | A life trail (Ending.trail) on the wire.
trail :: Codec.Codec [(Natural, [(PlayerId.PlayerId, Integer)])]
trail = Common.list (Common.tuple Common.natural (Common.list (Common.tuple Codec.PlayerId.codec Common.integer)))

-- | One end of a data channel: a message out, and the next one in, Nothing
-- once the channel closed or sent a line that does not read.
data Channel m = MkChannel
  { send :: Message -> m (),
    receive :: m (Maybe Message)
  }

-- | The host's side of a guest's seat (Protocol.Exchange): each request sent
-- with the time banks `clocks` reads then, answered by the reply that comes
-- back, any other message (a Ready toggled
-- as the game began, a rejoining guest's Hello) skipped. Once the channel
-- closes, `rejoin` connects it anew and the guest is sent `places`, the
-- match-up its history names, without the options, which a game under way
-- has no use for, its seat with what `seen` says it has seen, and the
-- request again, which a guest whose page reloaded needs all of; the
-- end of input, which stops the game, only where it does not.
remote :: (Monad m) => m Bool -> [Place] -> m Ending.Seen -> m Banks -> Channel m -> Protocol.Exchange m
remote rejoin places seen clocks channel request = asked
  where
    asked = clocks >>= send channel . MkAsk request >> replied
    replied = do
      heard <- receive channel
      case heard of
        Just (MkAnswer reply) -> pure (Just (Right reply))
        Just _ -> replied
        Nothing -> do
          back <- rejoin
          if back then seen >>= \seen' -> send channel (MkMatchup places []) >> send channel (MkStart (Question.seat (Request.question request)) seen') >> asked else pure Nothing

-- | This seat's view of the game as it ended, holding what happened after
-- its last decision (the killing blow, say); Nothing for a game that stopped.
endView :: PlayerId.PlayerId -> Either a (b, GameState.GameState) -> Maybe PlayerView.PlayerView
endView seat = either (const Nothing) (Just . PlayerView.Engine.viewFor seat . snd)

-- | A seat's exchange, each request's view given to `sent` once answered:
-- what the host keeps of a guest's seat, whose page may reload and lose it.
-- Until then the guest rejoining is sent what it saw before the request
-- (remote), so the request, sent again, shows its news as fresh.
seeing :: (Monad m) => (PlayerView.PlayerView -> m ()) -> Protocol.Exchange m -> Protocol.Exchange m
seeing sent ask request = ask request <* mapM_ sent (Request.view request)

-- | The ending of seat `seat`, from what it was shown, `seen`, and its view
-- of the end (endView), the killing blow, say, taking no seat's side where
-- `shared`.
ending :: Ending.Verdict -> [Text.Text] -> Text.Text -> PlayerId.PlayerId -> Bool -> Either a (b, GameState.GameState) -> Ending.Seen -> Ending.Ending
ending verdict said scenario seat shared outcome seen = Ending.endingOf verdict said scenario seat shared (maybe id Ending.see (endView seat outcome) seen)

-- | What a guest was last told, or shown by a request of its own: the seat
-- deciding, and the game as the guest's seat saw it then (MkDeciding).
type Told = (PlayerId.PlayerId, PlayerView.PlayerView, Natural)

-- | Tells each guest, over its channel in `guests`, of the seat `seat`
-- deciding, another's than its own, with the game `gs` at depth `depth` as
-- its seat sees it and the time banks `clocks`, unless `told` holds that
-- already, its clocks aside: a guest's own seat asked counts, since it is
-- sent the request. What each was told of then.
telling :: (Monad m) => Map.Map PlayerId.PlayerId (Channel m) -> PlayerId.PlayerId -> GameState.GameState -> Natural -> Banks -> Map.Map PlayerId.PlayerId Told -> m (Map.Map PlayerId.PlayerId Told)
telling guests seat gs depth clocks told = Map.traverseWithKey tell guests
  where
    tell g channel = do
      let shown = (PlayerView.Engine.viewFor g gs) {PlayerView.priority = Nothing}
          now = (seat, shown, depth)
      Monad.when (g /= seat && Map.lookup g told /= Just now) (send channel (MkDeciding seat shown depth clocks))
      pure now

-- | Each prompt's deciding seat given to `decides` before it is answered,
-- whoever answers it, a bot included, with the game then and its depth
-- (Protocol.depthOf).
deciding :: (Monad m) => (PlayerId.PlayerId -> GameState.GameState -> Natural -> m ()) -> Observer.Observer (Game.Play m)
deciding decides =
  Observer.MkObserver
    { Observer.before = \asked -> mapM_ (\seat -> Game.underneath (decides seat (Asked.game asked) (Protocol.depthOf asked))) (Driver.deciderOf (Asked.prompt asked)),
      Observer.after = \_ _ -> pure ()
    }

-- | What the guest does with a message from the host once the game is under
-- way: a request is answered through `local`, its reply what to send back;
-- the end is said; whose decision the game waits on is shown, on the board
-- as the guest's seat then sees it, at that depth, with the clocks then;
-- anything else is ignored.
data Heard
  = MkReply Message
  | MkWaitingOn PlayerId.PlayerId PlayerView.PlayerView Natural Banks
  | MkEnded Ending.Ending
  | MkIgnored
  deriving (Eq, Show)

-- | The guest's side of its seat: each request the host sends answered by
-- `local`, the page's own exchange, given the clocks sent with it, and
-- nothing else ever answering it. A local exchange that ends (Nothing)
-- answers nothing: the host waits on until the channel closes, or tells the
-- game's end (Guest.play).
guest :: (Monad m) => (Banks -> Protocol.Exchange m) -> Message -> m Heard
guest local message = case message of
  MkAsk request clocks -> maybe MkIgnored (either (const MkIgnored) (MkReply . MkAnswer)) <$> local clocks request
  MkEnd ending' -> pure (MkEnded ending')
  MkDeciding seat shown depth clocks -> pure (MkWaitingOn seat shown depth clocks)
  _ -> pure MkIgnored

-- | A game as the page playing it keeps it (the host's, for a game with
-- guests), to pick it up after a reload (Protocol.resumed): its seed, where
-- it began, how each bot plays by its slot (Parun.Play.Opponent;
-- Lobby.botAt), and every reply so far, each with every timed seat's time
-- left once it was made (Parun.Web.Clock.resumed).
data Saved = MkSaved
  { seed :: Key.Type.Key,
    origin :: Origin,
    bots :: Map.Map Int (Difficulty.Difficulty, Persona.Persona),
    replies :: [(R.Reply, Banks)]
  }
  deriving (Eq, Show)

-- | Where a kept game began.
data Origin
  = -- | Dealt from the lobby.
    MkLobby Dealt
  | -- | Played on from this position of this replay, each seat filled so
    -- (Parun.Browser.Replay.playOn).
    MkReplayed Web.Replay.Source Natural (Map.Map PlayerId.PlayerId Web.Replay.Filling)
  deriving (Eq, Show)

-- | A game dealt from the lobby: each seat's deck as a label and its list's
-- text (the host's first), the bots' slots players were invited to, those
-- played on the host's screen and how, the options the game is played with
-- (CR 800.2) but its format, which its lists' headers name, and the
-- minutes each seat a person plays has on its clock (Parun.Web.Clock), if
-- timed.
data Dealt = MkDealt
  { seats :: NonEmpty.NonEmpty (Text.Text, Text.Text),
    invited :: [Int],
    locals :: Map.Map Int Sitting,
    settings :: GameSettings.GameSettings,
    clock :: Maybe Natural
  }
  deriving (Eq, Show)

-- | How a bot's slot played on the host's screen is played.
data Sitting
  = -- | By another player there, handed the screen before each of their
    -- questions (Parun.Web.Question.handOff).
    MkHandedOver
  | -- | By the host too, with no hand-off between the host's seats: a
    -- goldfish game, one person deciding for every seat.
    MkYours
  deriving (Eq, Ord, Show)

-- | A saved game as the page keeps it, stamped with the build that played
-- it: a line for the game, then one per reply (savedReply), so a reply is
-- kept by adding its line. A replay played on is kept as its route
-- (Parun.Web.Route).
saved :: Text.Text -> Saved -> Text.Text
saved build game =
  let seat (name, contents) = Value.object [Value.pair "deck" (Value.text name), Value.pair "list" (Value.text contents)]
      begun = case origin game of
        MkLobby dealt ->
          let slots sitting = Value.array [Value.integer (toInteger i) | (i, how) <- Map.toList (locals dealt), how == sitting]
           in [ Value.pair "seats" (Value.array (fmap seat (NonEmpty.toList (seats dealt)))),
                Value.pair "invited" (Value.array (fmap (Value.integer . toInteger) (invited dealt))),
                Value.pair "locals" (slots MkHandedOver),
                Value.pair "yours" (slots MkYours),
                Value.pair "settings" (Codec.encode Codec.GameSettings.codec (settings dealt)),
                Value.pair "clock" (Codec.encode (Common.maybe Common.natural) (clock dealt))
              ]
        MkReplayed source at chosen -> [Value.pair "replay" (Value.text (Route.render (Route.MkReplay (Route.MkPlayingOn chosen) source at)))]
      header =
        Value.object
          ( [ Value.pair "build" (Value.text build),
              Value.pair "seed" (Value.text (Key.toHex (seed game))),
              Value.pair "bots" (Value.array [Value.object [Value.pair "slot" (Value.integer (toInteger i)), Value.pair "difficulty" (Value.string (fst (Opponent.difficultyName level))), Value.pair "persona" (Value.string (fst (Opponent.personaName style)))] | (i, (level, style)) <- Map.toList (bots game)])
            ]
              <> begun
          )
   in Text.intercalate (Text.pack "\n") (Common.render header : fmap savedReply (replies game))

-- | A reply's line in a saved game, with the time banks then.
savedReply :: (R.Reply, Banks) -> Text.Text
savedReply (reply, clocks) = Common.render (Value.object [Value.pair "reply" (Codec.Reply.toValue reply), Value.pair "clocks" (Codec.encode banksCodec clocks)])

-- | The game a page kept, or why it does not read: one another build played
-- is refused, since its replies may not replay the same game.
unsaved :: Text.Text -> Text.Text -> Either Text.Text Saved
unsaved build kept = case Text.lines kept of
  [] -> Left (Text.pack "no game")
  first : rest -> do
    pairs <- Common.parse first >>= Common.asObject
    theirs <- Common.field "build" pairs >>= Common.asText
    Monad.unless (theirs == build) . Left $ Text.pack "the game was played on another build of Parun"
    key <- Common.field "seed" pairs >>= Common.asText >>= maybe (Left (Text.pack "not a seed")) Right . Key.fromHex
    begun <- maybe (dealt pairs) replayed (Common.optionalField "replay" pairs)
    styles <- Common.field "bots" pairs >>= Common.asArray >>= traverse styleOf
    answers <- traverse (Common.parse Monad.>=> Common.asObject Monad.>=> replyOf) (filter (not . Text.null) rest)
    pure (MkSaved key begun (Map.fromList styles) answers)
  where
    named :: (Bounded a, Enum a) => String -> (a -> (String, String)) -> Text.Text -> Either Text.Text a
    named what nameOf name = maybe (Left (Text.pack ("not " <> what <> ": ") <> name)) Right (Opponent.named nameOf (Text.unpack name))
    replyOf fields = (,) <$> (Common.field "reply" fields >>= Codec.Reply.fromValue) <*> (Common.field "clocks" fields >>= Codec.decode banksCodec)
    seat value = do
      fields <- Common.asObject value
      (,) <$> (Common.field "deck" fields >>= Common.asText) <*> (Common.field "list" fields >>= Common.asText)
    styleOf value = do
      fields <- Common.asObject value
      i <- Common.field "slot" fields >>= Common.asInteger >>= maybe (Left (Text.pack "not a slot")) Right . Integer.toInt
      level <- Common.field "difficulty" fields >>= Common.asText >>= named "a difficulty" Opponent.difficultyName
      style <- Common.field "persona" fields >>= Common.asText >>= named "a persona" Opponent.personaName
      pure (i, (level, style))
    replayed value = do
      route <- Common.asText value
      case Route.parse route of
        Route.MkReplay (Route.MkPlayingOn chosen) source at -> Right (MkReplayed source at chosen)
        _ -> Left (Text.pack "not a replay played on: " <> route)
    dealt header = do
      listed <- Common.field "seats" header >>= Common.asArray >>= traverse seat
      decks <- maybe (Left (Text.pack "no seats")) Right (NonEmpty.nonEmpty listed)
      let slotsAt name = Common.field name header >>= Common.asArray >>= traverse (Common.asInteger Monad.>=> maybe (Left (Text.pack "not a slot")) Right . Integer.toInt)
      slots <- slotsAt "invited"
      handed <- slotsAt "locals"
      yours <- slotsAt "yours"
      options <- Common.field "settings" header >>= Codec.decode Codec.GameSettings.codec
      minutes <- Common.field "clock" header >>= Codec.decode (Common.maybe Common.natural)
      pure (MkLobby (MkDealt decks slots (Map.fromList (fmap (\i -> (i, MkHandedOver)) handed <> fmap (\i -> (i, MkYours)) yours)) options minutes))
