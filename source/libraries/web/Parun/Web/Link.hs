-- | A seat played from another browser (#9185): the messages the hosting page
-- and a guest's page send each other over their data channel, each a line of
-- JSON tagged with its type and the build that sent it, and both ends of a
-- game played over them. The host runs the engine; the guest only answers
-- the requests its seat is sent.
module Parun.Web.Link where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Types.PlayerId as PlayerId
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
  | -- | The guest's reply to the last request.
    MkAnswer R.Reply
  | -- | The game ended: how for the guest, what is said of it
    -- (Ending.said), and the game as a scenario file (Ending.scenario).
    MkEnd Ending.Verdict [Text.Text] Text.Text
  deriving (Eq, Show)

-- | A seat of the match-up as the guest is shown it.
data Place = MkPlace
  { -- | Who plays it: "Host", "You", "Bot 2".
    who :: Text.Text,
    -- | Its deck as the match-up names it (Lobby.label), or what stands for
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
        MkAnswer reply -> ("answer", [Value.pair "reply" (Codec.Reply.toValue reply)])
        MkEnd verdict said scenario -> ("end", [Value.pair "verdict" (Value.string (verdictName verdict)), Value.pair "said" (Value.array (fmap Value.text said)), Value.pair "scenario" (Value.text scenario)])
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
    "answer" -> MkAnswer <$> (Common.field "reply" pairs >>= Codec.Reply.fromValue)
    "end" -> MkEnd <$> (text "verdict" >>= verdictNamed) <*> (Common.field "said" pairs >>= Common.asArray >>= traverse Common.asText) <*> text "scenario"
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
-- as the game began) skipped; the end of input once the channel closes,
-- which stops the game.
remote :: (Monad m) => Channel m -> Protocol.Exchange m
remote channel request = send channel (MkAsk request) >> replied
  where
    replied = do
      heard <- receive channel
      case heard of
        Just (MkAnswer reply) -> pure (Just (Right reply))
        Just _ -> replied
        Nothing -> pure Nothing

-- | What the guest does with a message from the host once the game is under
-- way: a request is answered through `local`, its reply what to send back;
-- the end is said; anything else is ignored.
data Heard
  = MkReply Message
  | MkEnded Ending.Verdict [Text.Text] Text.Text
  | MkIgnored
  deriving (Eq, Show)

-- | The guest's side of its seat: each request the host sends answered by
-- `local`, the page's own exchange, and nothing else ever answering it. A
-- local exchange that ends (Nothing) answers nothing, which leaves the host
-- waiting until the channel closes.
guest :: (Monad m) => Protocol.Exchange m -> Message -> m Heard
guest local message = case message of
  MkAsk request -> maybe MkIgnored (either (const MkIgnored) (MkReply . MkAnswer)) <$> local request
  MkEnd verdict said scenario -> pure (MkEnded verdict said scenario)
  _ -> pure MkIgnored
