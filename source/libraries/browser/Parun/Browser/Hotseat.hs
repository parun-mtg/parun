-- | A game against the random bot in the browser: the lobby picks each deck
-- from the data the page mounted, then `parun play --json`'s game runs with
-- every request line answered on the page (Parun.Web.Question), and the
-- page ends on how it ended (Parun.Web.Ending).
module Parun.Browser.Hotseat where

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
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.DeckList as DeckList
import qualified Parun.Decks as Decks
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Client as Client
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Line as Line
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question

-- | The game on a page mounted first, so any exception that escapes it is
-- shown there rather than ending the module unseen.
start :: IO ()
start = do
  page <- Browser.mount
  Exception.catch (play page) $ \problem ->
    Browser.display page . message $ fmap Text.pack ["The game stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

play :: Browser.Page (Either Line.Line Web.Question.Event) -> IO ()
play page = do
  root <- Registry.defaultRoot
  loaded <- fmap (Registry.loadBundle root) $ Registry.bundlePath >>= ByteString.readFile
  deckRoot <- Decks.defaultRoot
  paths <- Decks.listRoot deckRoot
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
  seatRef <- IORef.newIORef . MkSeat (Just player) False Map.empty Seq.empty =<< Clock.getMonotonicTime
  listed <- (,) <$> Command.load registry one <*> Command.load registry two
  case listed of
    (Right first, Right second) -> do
      settled <- Command.settle Nothing [(one, first), (two, second)]
      case settled of
        Right format -> do
          seed <- Key.fresh
          (outcome, recorded) <- Command.game (exchange (Identity.runIdentity . Interpreter.chosenCard registry) page seatRef) options registry format (DeckList.deck first, DeckList.deck second) seed
          -- What the player saw after their last decision: the killing blow, say.
          Monad.forM_ outcome $ IORef.modifyIORef' seatRef . witness . PlayerView.Engine.viewFor player . snd
          seen <- logged <$> IORef.readIORef seatRef
          Browser.display page . Ending.view $ Ending.MkEnding (either (\reason -> [Text.pack "The game stopped: " <> reason <> Text.pack "."]) (\(result, gs) -> Render.result result : Render.departures gs) outcome) (Command.rendered recorded) seen
        Left problems -> say problems
    (first, second) -> say $ Either.fromLeft [] first <> Either.fromLeft [] second

-- | The seat the page plays.
player :: PlayerId.PlayerId
player = PlayerId.MkPlayerId 0

-- | The player is seat 1, over the line protocol; the random bot is seat 2.
options :: PlayOptions.PlayOptions
options = PlayOptions.none {PlayOptions.json = True, PlayOptions.bots = Set.singleton 2}

-- | One request answered on the page, as values (Protocol.Exchange): a reply the seat is never asked
-- for, or auto's next source once the seat chose auto for this payment, at
-- once; else after a hand-off when the screen last faced another seat, the
-- question's form until it answers, a card tapped showing its text from the
-- card data through `lookUp`.
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page (Either Line.Line Web.Question.Event) -> IORef.IORef Seat -> Protocol.Exchange IO
exchange lookUp page seatRef request = do
  arrived <- Clock.getMonotonicTime
  fmap (Just . Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    state <- IORef.readIORef seatRef
    let auto = if paying state then Web.Question.auto request else Nothing
        kind = Question.kind $ Request.question request
        traced how = Browser.trace page $ kind <> Text.pack (": engine " <> Browser.milliseconds (arrived - answeredAt state) <> ", " <> how)
    IORef.writeIORef seatRef state {paying = Maybe.isJust auto}
    reply <- case (Web.Question.automatic request, auto) of
      (Just reply, _) -> traced "answered unasked" >> pure reply
      (_, Just reply) -> traced "paid by auto" >> pure reply
      _ -> do
        traced "asked"
        let seat = Question.seat $ Request.question request
        Monad.when (facing state /= Just seat) $ do
          Browser.display page . fmap Right $ Web.Question.handOff seat
          untilReady
          IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        answer request Set.empty
    returned <- Clock.getMonotonicTime
    IORef.modifyIORef' seatRef $ \s -> s {answeredAt = returned}
    pure reply
  where
    untilReady = Browser.next page >>= \(event, _) -> Monad.unless (event == Right Web.Question.MkReady) untilReady
    answer pending picked = asking pending picked []
    asking pending picked reading = do
      Browser.display page . fmap Right $ Web.Question.view pending picked reading
      (event, value) <- Browser.next page
      case event of
        Right (Web.Question.MkRead object)
          | Just shown <- Request.view pending,
            Just o <- Map.lookup object (Render.objects shown) ->
              asking pending picked . either (pure . Text.pack . Exception.displayException) id $ Render.details lookUp shown o
        Right e -> case Web.Question.step pending picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = True}
            pure answered
          Web.Question.MkPicking now -> asking pending now []
          Web.Question.MkRefused problem -> asking pending {Request.problem = Just problem} picked []
        Left _ -> asking pending picked reading

-- | What the page keeps between request lines: the seat the screen last
-- showed, whether the seat chose auto for the payment under way, what the
-- seat has seen happen, and when it last answered.
data Seat = MkSeat
  { facing :: Maybe PlayerId.PlayerId,
    paying :: Bool,
    -- | How far the seat has read each game's log (Client.unseen).
    marks :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural),
    -- | The entries read so far, oldest first: the game log the end shows.
    logged :: Seq.Seq Text.Text,
    -- | When the last line was answered, the engine's work since then being
    -- what the trace times.
    answeredAt :: Double
  }

-- | The seat's state with the view's entries it has not read yet logged.
witness :: PlayerView.PlayerView -> Seat -> Seat
witness view state =
  let (moved, unread) = Client.unseen (marks state) view
   in state {marks = moved, logged = logged state <> Seq.fromList unread}

message :: [Text.Text] -> Html.Html msg
message said = Html.element (Text.pack "main") [] [Html.element (Text.pack "p") [] [Html.text t] | t <- said]
