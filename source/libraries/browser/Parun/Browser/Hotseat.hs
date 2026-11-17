-- | A game against the random bot in the browser: the lobby picks each deck
-- from the data the page mounted, then `parun play --json`'s game runs with
-- every request line answered on the page (Parun.Web.Question).
module Parun.Browser.Hotseat where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Browser as Browser
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Command as Command
import qualified Parun.Registry as Registry
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
  facing <- IORef.newIORef . Just $ PlayerId.MkPlayerId 0
  listed <- (,) <$> Command.load registry one <*> Command.load registry two
  case listed of
    (Right first, Right second) -> do
      settled <- Command.settle Nothing [(one, first), (two, second)]
      case settled of
        Right format -> do
          Command.play (exchange page facing) options registry format (DeckList.deck first, DeckList.deck second)
          say [Text.pack "The game is over."]
        Left problems -> say problems
    (first, second) -> say $ Either.fromLeft [] first <> Either.fromLeft [] second

-- | The player is seat 1, over the line protocol; the random bot is seat 2.
options :: PlayOptions.PlayOptions
options = PlayOptions.none {PlayOptions.json = True, PlayOptions.bots = Set.singleton 2}

-- | One request line answered on the page: a reply the seat is never asked
-- for at once, else after a hand-off when the screen last faced another seat,
-- the question's form until it answers. Nothing for a line that does not read.
exchange :: Browser.Page (Either Line.Line Web.Question.Event) -> IORef.IORef (Maybe PlayerId.PlayerId) -> Text.Text -> IO (Maybe Text.Text)
exchange page facing line = case Common.parse line >>= Codec.decode Codec.Request.codec of
  Left _ -> pure Nothing
  Right request -> fmap (Just . Common.render . Codec.Reply.toValue) $ case Web.Question.automatic request of
    Just reply -> pure reply
    Nothing -> do
      let seat = Question.seat $ Request.question request
      before <- IORef.readIORef facing
      Monad.when (before /= Just seat) $ do
        Browser.display page . fmap Right $ Web.Question.handOff seat
        Monad.void . untilReady $ Browser.next page
        IORef.writeIORef facing $ Just seat
      answer request Set.empty
  where
    untilReady wait = wait >>= \(event, _) -> if event == Right Web.Question.MkReady then pure () else untilReady wait
    answer request picked = do
      Browser.display page . fmap Right $ Web.Question.view request picked
      (event, value) <- Browser.next page
      case event of
        Right e -> case Web.Question.step request picked e value of
          Web.Question.MkReplied reply -> pure reply
          Web.Question.MkPicking now -> answer request now
          Web.Question.MkRefused problem -> answer request {Request.problem = Just problem} picked
        Left _ -> answer request picked

message :: [Text.Text] -> Html.Html msg
message said = Html.element (Text.pack "main") [] [Html.element (Text.pack "p") [] [Html.text t] | t <- said]
