-- | A game against the random bot in the browser: the lobby picks each deck
-- from the data the page mounted, then `parun play --json`'s game runs with
-- every request line answered on the page (Parun.Web.Question).
module Parun.Browser.Hotseat where

import Control.Applicative ((<|>))
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Browser as Browser
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Line as Line
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question

start :: IO ()
start = do
  root <- Registry.defaultRoot
  loaded <- fmap (Registry.loadBundle root) $ Registry.bundlePath >>= ByteString.readFile
  deckRoot <- Decks.defaultRoot
  paths <- Decks.listRoot deckRoot
  page <- Browser.mount
  let byName = Map.fromList [(Lobby.deckName deckRoot path, path) | path <- paths]
      lobby = Lobby.MkLobby (Natural.length loaded) (Seq.fromList $ Map.keys byName) Nothing
      pick chosen = do
        Browser.display page . fmap Left $ Lobby.view lobby {Lobby.first = chosen}
        (event, value) <- Browser.next page
        case event of
          Left line | Just path <- Map.lookup (Line.resolve line value) byName -> pure path
          _ -> pick chosen
      say = Browser.display page . fmap Right . message
  one <- pick Nothing
  two <- pick . Just $ Lobby.deckName deckRoot one
  -- The printing table is the slow part of the registry, so the lobby shows first.
  say [Text.pack "Starting the game\x2026"]
  registry <- Registry.defaultRegistry loaded :: IO (Registry.Registry Identity.Identity)
  -- The bot needs no screen, so the player is never handed it.
  seatRef <- IORef.newIORef $ MkSeat (Just $ PlayerId.MkPlayerId 0) False
  listed <- (,) <$> Command.load registry one <*> Command.load registry two
  case listed of
    (Right first, Right second) -> do
      settled <- Command.settle Nothing [(one, first), (two, second)]
      case settled of
        Right format -> do
          Command.play (exchange (Identity.runIdentity . Interpreter.chosenCard registry) page seatRef) options registry format (DeckList.deck first, DeckList.deck second)
          say [Text.pack "The game is over."]
        Left problems -> say problems
    (first, second) -> say $ Either.fromLeft [] first <> Either.fromLeft [] second

-- | The player is seat 1, over the line protocol; the random bot is seat 2.
options :: PlayOptions.PlayOptions
options = PlayOptions.none {PlayOptions.json = True, PlayOptions.bots = Set.singleton 2}

-- | One request line answered on the page: a reply the seat is never asked
-- for, or auto's next source once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat, the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`. Nothing for a line that does not read.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page (Either Line.Line Web.Question.Event) -> IORef.IORef Seat -> Text.Text -> IO (Maybe Text.Text)
exchange lookUp page seatRef line = case Common.parse line >>= Codec.decode Codec.Request.codec of
  Left _ -> pure Nothing
  Right request -> fmap (Just . Common.render . Codec.Reply.toValue) $ do
    state <- IORef.readIORef seatRef
    let auto = if paying state then Web.Question.auto request else Nothing
    IORef.writeIORef seatRef state {paying = Maybe.isJust auto}
    case Web.Question.automatic request <|> auto of
      Just reply -> pure reply
      Nothing -> do
        let seat = Question.seat $ Request.question request
        Monad.when (facing state /= Just seat) $ do
          Browser.display page . fmap Right $ Web.Question.handOff seat
          untilReady
          IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        answer request Set.empty
  where
    untilReady = Browser.next page >>= \(event, _) -> Monad.unless (event == Right Web.Question.MkReady) untilReady
    answer request picked = asking request picked []
    asking request picked reading = do
      Browser.display page . fmap Right $ Web.Question.view request picked reading
      (event, value) <- Browser.next page
      case event of
        Right (Web.Question.MkRead object)
          | Just shown <- Request.view request,
            Just o <- Map.lookup object (Render.objects shown) ->
              asking request picked . either (pure . Text.pack . Exception.displayException) id $ Render.details lookUp shown o
        Right e -> case Web.Question.step request picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = True}
            pure answered
          Web.Question.MkPicking now -> asking request now []
          Web.Question.MkRefused problem -> asking request {Request.problem = Just problem} picked []
        Left _ -> asking request picked reading

-- | What the page keeps between request lines: the seat the screen last
-- showed, and whether the seat chose auto for the payment under way.
data Seat = MkSeat
  { facing :: Maybe PlayerId.PlayerId,
    paying :: Bool
  }

message :: [Text.Text] -> Html.Html msg
message said = Html.element (Text.pack "main") [] [Html.element (Text.pack "p") [] [Html.text t] | t <- said]
