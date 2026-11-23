-- | A seat played from another browser (#9185): the messages the hosting page
-- and a guest's page send each other over their data channel, each a line of
-- JSON tagged with its type and the build that sent it, and both ends of a
-- game played over them. The host runs the engine; the guest only answers
-- the requests its seat is sent.
module Parun.Web.Link where

import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parun.Codec.PlayerView as Codec.PlayerView
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Difficulty as Difficulty
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

-- | What one page tells the other.
data Message
  = -- | The guest's deck, by its name and its list's text; sent again on a
    -- change of deck.
    MkHello Text.Text Text.Text
  | -- | The match-up as it stands, sent to the guest at every change.
    MkMatchup [Place]
  | -- | The guest is ready to start, or no longer.
    MkReady Bool
  | -- | The game starts, the guest playing this seat.
    MkStart PlayerId.PlayerId
  | -- | A request the guest's seat answers.
    MkAsk Request.Request
  | -- | The seat whose decision the game now waits on, another's than the
    -- guest's (telling).
    MkDeciding PlayerId.PlayerId
  | -- | The guest's reply to the last request.
    MkAnswer R.Reply
  | -- | The game ended: how for the guest, what is said of it
    -- (Ending.said), the guest's seat's view of the end (endView), and the
    -- game as a scenario file (Ending.scenario).
    MkEnd Ending.Verdict [Text.Text] (Maybe PlayerView.PlayerView) Text.Text
  deriving (Eq, Show)

-- | A seat of the match-up as the guest is shown it.
data Place = MkPlace
  { -- | Who plays it: "Host", "You", "Bot 2".
    who :: Text.Text,
    -- | Its deck as the match-up names it (DeckBrowser.label), or what stands for
    -- one not chosen yet.
    deck :: Text.Text,
    -- | Whether the person playing it is ready; Nothing for a bot.
    ready :: Maybe Bool
  }
  deriving (Eq, Show)

-- | A message as a line, stamped with the build that sends it.
encode :: Text.Text -> Message -> Text.Text
encode build message =
  let (tag, fields) = case message of
        MkHello name contents -> ("hello", [Value.pair "name" (Value.text name), Value.pair "list" (Value.text contents)])
        MkMatchup places -> ("matchup", [Value.pair "seats" (Value.array (fmap place places))])
        MkReady on -> ("ready", [Value.pair "ready" (Value.boolean on)])
        MkStart seat -> ("start", [Value.pair "seat" (Value.integer (toInteger (PlayerId.unwrap seat)))])
        MkAsk request -> ("ask", [Value.pair "request" (Codec.encode Codec.Request.codec request)])
        MkDeciding seat -> ("deciding", [Value.pair "seat" (Value.integer (toInteger (PlayerId.unwrap seat)))])
        MkAnswer reply -> ("answer", [Value.pair "reply" (Codec.Reply.toValue reply)])
        MkEnd verdict said shown scenario -> ("end", [Value.pair "verdict" (Value.string (verdictName verdict)), Value.pair "said" (Value.array (fmap Value.text said)), Value.pair "view" (Codec.encode (Common.maybe Codec.PlayerView.codec) shown), Value.pair "scenario" (Value.text scenario)])
      place p = Value.object [Value.pair "who" (Value.text (who p)), Value.pair "deck" (Value.text (deck p)), Value.pair "ready" (maybe Value.null Value.boolean (ready p))]
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
  case Text.unpack tag of
    "hello" -> MkHello <$> text "name" <*> text "list"
    "matchup" -> MkMatchup <$> (Common.field "seats" pairs >>= Common.asArray >>= traverse place)
    "ready" -> MkReady <$> (Common.field "ready" pairs >>= Common.asBoolean)
    "start" -> MkStart <$> (Common.field "seat" pairs >>= Common.asInteger >>= natural)
    "ask" -> MkAsk <$> (Common.field "request" pairs >>= Codec.decode Codec.Request.codec)
    "deciding" -> MkDeciding <$> (Common.field "seat" pairs >>= Common.asInteger >>= natural)
    "answer" -> MkAnswer <$> (Common.field "reply" pairs >>= Codec.Reply.fromValue)
    "end" -> MkEnd <$> (text "verdict" >>= verdictNamed) <*> (Common.field "said" pairs >>= Common.asArray >>= traverse Common.asText) <*> (Common.field "view" pairs >>= Codec.decode (Common.maybe Codec.PlayerView.codec)) <*> text "scenario"
    other -> Left (Text.pack ("not a message: " <> other))
  where
    place value = do
      fields <- Common.asObject value
      MkPlace
        <$> (Common.field "who" fields >>= Common.asText)
        <*> (Common.field "deck" fields >>= Common.asText)
        <*> maybe (Right Nothing) (\v -> if v == Value.null then Right Nothing else Just <$> Common.asBoolean v) (Common.optionalField "ready" fields)
    natural :: Integer -> Either Text.Text PlayerId.PlayerId
    natural = maybe (Left (Text.pack "not a seat")) (Right . PlayerId.MkPlayerId) . Integer.toNatural

verdictName :: Ending.Verdict -> String
verdictName verdict = case verdict of
  Ending.MkWon -> "won"
  Ending.MkLost -> "lost"
  Ending.MkDrawn -> "drawn"
  Ending.MkStopped -> "stopped"

verdictNamed :: Text.Text -> Either Text.Text Ending.Verdict
verdictNamed name = case Text.unpack name of
  "won" -> Right Ending.MkWon
  "lost" -> Right Ending.MkLost
  "drawn" -> Right Ending.MkDrawn
  "stopped" -> Right Ending.MkStopped
  _ -> Left (Text.pack "not a verdict: " <> name)

-- | One end of a data channel: a message out, and the next one in, Nothing
-- once the channel closed or sent a line that does not read.
data Channel m = MkChannel
  { send :: Message -> m (),
    receive :: m (Maybe Message)
  }

-- | The host's side of a guest's seat (Protocol.Exchange): each request sent,
-- answered by the reply that comes back, any other message (a Ready toggled
-- as the game began, a rejoining guest's Hello) skipped. Once the channel
-- closes, `rejoin` connects it anew and the guest is sent `places`, the
-- match-up its history names, its seat and the request again, which a guest
-- whose page reloaded needs all of; the end of input, which stops the game,
-- only where it does not.
remote :: (Monad m) => m Bool -> [Place] -> Channel m -> Protocol.Exchange m
remote rejoin places channel request = asked
  where
    asked = send channel (MkAsk request) >> replied
    replied = do
      heard <- receive channel
      case heard of
        Just (MkAnswer reply) -> pure (Just (Right reply))
        Just _ -> replied
        Nothing -> do
          back <- rejoin
          if back then send channel (MkMatchup places) >> send channel (MkStart (Question.seat (Request.question request))) >> asked else pure Nothing

-- | This seat's view of the game as it ended, holding what happened after
-- its last decision (the killing blow, say); Nothing for a game that stopped.
endView :: PlayerId.PlayerId -> Either a (b, GameState.GameState) -> Maybe PlayerView.PlayerView
endView seat = either (const Nothing) (Just . PlayerView.Engine.viewFor seat . snd)

-- | Tells each guest, over its channel in `guests`, of every seat the game
-- asks but its own as the seat asked changes, `told` holding the seat each
-- was last told of: a guest's own seat asked counts, since it is sent the
-- request. What each was told of then.
telling :: (Monad m) => Map.Map PlayerId.PlayerId (Channel m) -> PlayerId.PlayerId -> Map.Map PlayerId.PlayerId PlayerId.PlayerId -> m (Map.Map PlayerId.PlayerId PlayerId.PlayerId)
telling guests seat told = do
  Monad.forM_ (Map.toList guests) $ \(g, channel) ->
    Monad.when (g /= seat && Map.lookup g told /= Just seat) (send channel (MkDeciding seat))
  pure (seat <$ guests)

-- | Each prompt's deciding seat given to `decides` before it is answered,
-- whoever answers it, a bot included.
deciding :: (Monad m) => (PlayerId.PlayerId -> m ()) -> Observer.Observer (Game.Play m)
deciding decides =
  Observer.MkObserver
    { Observer.before = mapM_ (Game.underneath . decides) . Driver.deciderOf . Asked.prompt,
      Observer.after = \_ _ -> pure ()
    }

-- | What the guest does with a message from the host once the game is under
-- way: a request is answered through `local`, its reply what to send back;
-- the end is said, with the seat's view of it; whose decision the game waits
-- on is shown; anything else is ignored.
data Heard
  = MkReply Message
  | MkWaitingOn PlayerId.PlayerId
  | MkEnded Ending.Verdict [Text.Text] (Maybe PlayerView.PlayerView) Text.Text
  | MkIgnored
  deriving (Eq, Show)

-- | The guest's side of its seat: each request the host sends answered by
-- `local`, the page's own exchange, and nothing else ever answering it. A
-- local exchange that ends (Nothing) answers nothing, which leaves the host
-- waiting until the channel closes.
guest :: (Monad m) => Protocol.Exchange m -> Message -> m Heard
guest local message = case message of
  MkAsk request -> maybe MkIgnored (either (const MkIgnored) (MkReply . MkAnswer)) <$> local request
  MkEnd verdict said shown scenario -> pure (MkEnded verdict said shown scenario)
  MkDeciding seat -> pure (MkWaitingOn seat)
  _ -> pure MkIgnored

-- | A game with guests as the host's page keeps it, to pick it up after a
-- reload (Protocol.resumed): its seed, each seat's deck as a label and its
-- list's text (the host's first), the bots' slots players were invited to,
-- those played on the host's screen and how, how every other bot plays
-- (Parun.Play.Opponent), and every reply so far.
data Saved = MkSaved
  { seed :: Key.Type.Key,
    seats :: NonEmpty.NonEmpty (Text.Text, Text.Text),
    invited :: [Int],
    locals :: Map.Map Int Sitting,
    difficulty :: Difficulty.Difficulty,
    persona :: Persona.Persona,
    replies :: [R.Reply]
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
-- kept by adding its line.
saved :: Text.Text -> Saved -> Text.Text
saved build game =
  let seat (name, contents) = Value.object [Value.pair "deck" (Value.text name), Value.pair "list" (Value.text contents)]
      slots sitting = Value.array [Value.integer (toInteger i) | (i, how) <- Map.toList (locals game), how == sitting]
      header =
        Value.object
          [ Value.pair "build" (Value.text build),
            Value.pair "seed" (Value.text (Key.toHex (seed game))),
            Value.pair "seats" (Value.array (fmap seat (NonEmpty.toList (seats game)))),
            Value.pair "invited" (Value.array (fmap (Value.integer . toInteger) (invited game))),
            Value.pair "locals" (slots MkHandedOver),
            Value.pair "yours" (slots MkYours),
            Value.pair "difficulty" (Value.string (fst (Opponent.difficultyName (difficulty game)))),
            Value.pair "persona" (Value.string (fst (Opponent.personaName (persona game))))
          ]
   in Text.intercalate (Text.pack "\n") (Common.render header : fmap savedReply (replies game))

-- | A reply's line in a saved game.
savedReply :: R.Reply -> Text.Text
savedReply = Common.render . Codec.Reply.toValue

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
    listed <- Common.field "seats" pairs >>= Common.asArray >>= traverse seat
    decks <- maybe (Left (Text.pack "no seats")) Right (NonEmpty.nonEmpty listed)
    let slotsAt name = Common.field name pairs >>= Common.asArray >>= traverse (Common.asInteger Monad.>=> maybe (Left (Text.pack "not a slot")) Right . Integer.toInt)
    slots <- slotsAt "invited"
    handed <- slotsAt "locals"
    yours <- slotsAt "yours"
    let here = Map.fromList (fmap (\i -> (i, MkHandedOver)) handed <> fmap (\i -> (i, MkYours)) yours)
    level <- Common.field "difficulty" pairs >>= Common.asText >>= named "a difficulty" Opponent.difficultyName
    style <- Common.field "persona" pairs >>= Common.asText >>= named "a persona" Opponent.personaName
    answers <- traverse (Common.parse Monad.>=> Codec.Reply.fromValue) (filter (not . Text.null) rest)
    pure (MkSaved key decks slots here level style answers)
  where
    named :: (Bounded a, Enum a) => String -> (a -> (String, String)) -> Text.Text -> Either Text.Text a
    named what nameOf name = maybe (Left (Text.pack ("not " <> what <> ": ") <> name)) Right (Opponent.named nameOf (Text.unpack name))
    seat value = do
      fields <- Common.asObject value
      (,) <$> (Common.field "deck" fields >>= Common.asText) <*> (Common.field "list" fields >>= Common.asText)
