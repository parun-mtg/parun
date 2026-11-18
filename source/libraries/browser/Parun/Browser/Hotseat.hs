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
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Integer as Integer
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
import qualified Parun.Types.Key as Key
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Result as Result
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question

-- | The game on a page mounted first, so any exception that escapes it is
-- shown there rather than ending the module unseen.
start :: IO ()
start = do
  page <- Browser.mount
  Exception.catch (play page) $ \problem ->
    Browser.display page . message $ fmap Text.pack ["The game stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

play :: Browser.Page (Either Lobby.Event Web.Question.Event) -> IO ()
play page = do
  root <- Registry.defaultRoot
  loaded <- fmap (Registry.loadBundle root) $ Registry.bundlePath >>= ByteString.readFile
  deckRoot <- Decks.defaultRoot
  paths <- Decks.listRoot deckRoot
  let byName = Map.fromList [(Lobby.deckName deckRoot path, path) | path <- paths]
      names = Seq.fromList (Map.keys byName)
      random = do
        key <- Key.fresh
        let roll = foldr (\w acc -> acc * 4294967296 + toInteger w) 0 (Key.unwrap key)
        pure (Integer.toInt (mod roll (toInteger (max 1 (Seq.length names)))) >>= (\i -> Seq.lookup i names))
      choose lobby = do
        Browser.display page . fmap Left $ Lobby.view lobby
        (event, value) <- Browser.next page
        case event of
          Left Lobby.MkStart | Just one <- Lobby.mine lobby >>= (`Map.lookup` byName) -> do
            two <- maybe (fmap (Maybe.fromMaybe Text.empty) random) pure (Lobby.theirs lobby)
            maybe (choose lobby) (\path -> pure (one, path)) (Map.lookup two byName)
          Left Lobby.MkRandom -> random >>= maybe (choose lobby) (\name -> choose (Lobby.update (Lobby.MkPick name) Text.empty lobby))
          Left e -> choose (Lobby.update e value lobby)
          Right _ -> choose lobby
      say = Browser.display page . fmap Right . message
  (one, two) <- choose (Lobby.new (Natural.length loaded) names)
  -- The printing table is the slow part of the registry, so the lobby shows first.
  Browser.display page . fmap Right $ starting (Lobby.deckName deckRoot one) (Lobby.deckName deckRoot two)
  registry <- Registry.defaultRegistry loaded :: IO (Registry.Registry Identity.Identity)
  -- The bot needs no screen, so the player is never handed it.
  seatRef <- IORef.newIORef . MkSeat (Just player) False Nothing Map.empty Seq.empty 0 =<< Clock.getMonotonicTime
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
          Browser.display page . Ending.view $ Ending.MkEnding (verdictOf outcome) (either (\reason -> [Text.pack "The game stopped: " <> reason <> Text.pack "."]) (\(result, gs) -> Render.result result : Render.departures gs) outcome) (Command.rendered recorded) seen
        Left problems -> say problems
    (first, second) -> say $ Either.fromLeft [] first <> Either.fromLeft [] second

-- | How the game ended for the seat the page plays.
verdictOf :: Either a (Result.Result, b) -> Ending.Verdict
verdictOf outcome = case outcome of
  Left _ -> Ending.MkStopped
  Right (Result.Won pid, _) -> if pid == player then Ending.MkWon else Ending.MkLost
  Right (Result.TeamWon _, _) -> Ending.MkStopped
  Right (Result.Drawn, _) -> Ending.MkDrawn

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
exchange :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Browser.Page (Either Lobby.Event Web.Question.Event) -> IORef.IORef Seat -> Protocol.Exchange IO
exchange lookUp page seatRef request = do
  arrived <- Clock.getMonotonicTime
  fmap (Just . Right) $ do
    Monad.forM_ (Request.view request) $ IORef.modifyIORef' seatRef . witness
    state <- IORef.readIORef seatRef
    let auto = if paying state then Web.Question.auto request else Nothing
        kind = Question.kind $ Request.question request
        traced how = Browser.trace page $ kind <> Text.pack (": engine " <> Browser.milliseconds (arrived - answeredAt state) <> ", " <> how)
        turn = fmap PlayerView.turn (Request.view request)
        -- The seat's own stop: it asked to pass until the turn ends, which
        -- holds while the stack is empty, and declares no attackers.
        skipped = do
          Monad.guard (Maybe.isJust (skipping state) && skipping state == turn)
          shown <- Request.view request
          Monad.guard (null (PlayerView.stack shown))
          case Text.unpack kind of
            "ChooseAction" -> fmap R.Number (Web.Question.passIndex request)
            "DeclareAttackers" -> Just (R.Array [])
            _ -> Nothing
    IORef.writeIORef seatRef state {paying = Maybe.isJust auto, skipping = if skipping state == turn then skipping state else Nothing}
    reply <- case (Web.Question.automatic request, auto, skipped) of
      (Just reply, _, _) -> traced "answered unasked" >> pure reply
      (_, Just reply, _) -> traced "paid by auto" >> pure reply
      (_, _, Just reply) -> traced "skipped" >> pure reply
      _ -> do
        traced "asked"
        let seat = Question.seat $ Request.question request
        Monad.when (facing state /= Just seat) $ do
          Browser.display page . fmap Right $ Web.Question.handOff seat
          untilReady
          IORef.modifyIORef' seatRef $ \s -> s {facing = Just seat}
        answer request Set.empty
    returned <- Clock.getMonotonicTime
    IORef.modifyIORef' seatRef $ \s -> s {answeredAt = returned, decided = Seq.length (logged s)}
    pure reply
  where
    untilReady = Browser.next page >>= \(event, _) -> Monad.unless (event == Right Web.Question.MkReady) untilReady
    answer pending picked = asking pending picked Nothing Nothing
    asking pending picked reading focus = do
      state <- IORef.readIORef seatRef
      let screen = Web.Question.MkScreen picked reading (logged state) (Int.toNaturalSaturating (Seq.length (logged state) - decided state)) focus
      Browser.display page . fmap Right $ Web.Question.view pending screen
      (event, value) <- Browser.next page
      case event of
        Right (Web.Question.MkRead object)
          | Just shown <- Request.view pending,
            Just o <- Map.lookup object (Render.objects shown) ->
              (\r -> asking pending picked (Just r) focus) . Web.Card.MkReading object . either (pure . Text.pack . Exception.displayException) id $ Web.Card.oracle lookUp o
        Right e -> case Web.Question.step pending picked e value of
          Web.Question.MkReplied answered -> do
            Monad.when (e == Web.Question.MkAuto) . IORef.modifyIORef' seatRef $ \s -> s {paying = True}
            Monad.when (e == Web.Question.MkSkip) . IORef.modifyIORef' seatRef $ \s -> s {skipping = fmap PlayerView.turn (Request.view pending)}
            pure answered
          -- A pick made from the card's sheet leaves the sheet open.
          Web.Question.MkPicking now -> asking pending now (if isToggle e then reading else Nothing) (focusAfter e focus)
          Web.Question.MkRefused problem -> asking pending {Request.problem = Just problem} picked Nothing focus
        Left _ -> asking pending picked reading focus
    focusAfter e focus = case e of
      Web.Question.MkFocus g -> if focus == Just g then Nothing else Just g
      Web.Question.MkClear -> Nothing
      _ -> focus
    isToggle e = case e of
      Web.Question.MkToggle _ _ -> True
      Web.Question.MkFocus _ -> True
      _ -> False

-- | What the page keeps between request lines: the seat the screen last
-- showed, whether the seat chose auto for the payment under way, what the
-- seat has seen happen, and when it last answered.
data Seat = MkSeat
  { facing :: Maybe PlayerId.PlayerId,
    paying :: Bool,
    -- | The turn the seat asked to pass to the end of (Web.Question.MkSkip).
    skipping :: Maybe Natural,
    -- | How far the seat has read each game's log (Client.unseen).
    marks :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural),
    -- | The entries read so far, oldest first: the game log the end shows.
    logged :: Seq.Seq Text.Text,
    -- | How many entries were logged when the seat last answered.
    decided :: Int,
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
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Html.text t] | t <- said]

-- | While the registry builds and the game deals: the match-up, and a spinner.
starting :: Text.Text -> Text.Text -> Html.Html msg
starting one two =
  Element.el
    "main"
    ["message", "starting"]
    []
    [ Element.el "div" ["spinner"] [Element.attr "aria-hidden" (Text.pack "true")] [],
      Element.el "p" [] [] [Element.string "Shuffling up\x2026"],
      Element.el "p" ["matchup-line"] [] [Html.text (Lobby.label one), Element.el "span" ["versus"] [] [Element.string " vs "], Html.text (Lobby.label two)]
    ]
