-- | A scenario replayed on the page (#8821): each position reached by
-- replaying the timeline from the board with the test runner's own run
-- (Parun.Scenario.replay), shown as a chosen seat sees it
-- (Parun.Web.Replay), and played on by hand from one (#8871). The shell
-- (Parun.Browser.Shell) lists them and moves between these.
module Parun.Browser.Replay where

import Control.Applicative ((<|>))
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Identity as Identity
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Search as Search
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Load as Load
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Board as Board
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Label as Label
import qualified Parun.Types.LegalityTable as LegalityTable
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.ScenarioFailure as ScenarioFailure
import qualified Parun.Types.Seat as Seat
import qualified Parun.Types.Start as Start
import qualified Parun.Types.Stop as Stop
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.History as History
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Replay as Web.Replay
import qualified Parun.Web.Screen as Screen
import qualified Text.Read as Read

-- | The scenarios' names, the manifest's themes (or why it does not
-- decode), and each scenario's description from the titles
-- (Parun.Scenario.Load.titlesPath), none where they are missing or do not
-- decode, once the scenarios have arrived.
listing :: IO (Either Text.Text (Map.Map Text.Text [Text.Text]), Map.Map Text.Text Text.Text, [Text.Text])
listing = do
  root <- Load.defaultRoot
  paths <- Load.listRoot root
  themes <- Load.manifestPath >>= fmap Load.parseManifest . ByteString.readFile
  titles <- Load.titlesPath >>= Exception.try . ByteString.readFile
  let described = either (const Map.empty) (Either.fromRight Map.empty . Load.parseTitles) (titles :: Either Exception.IOException ByteString.ByteString)
  pure (themes, described, fmap (nameOf root) paths)

-- | A path's name beneath the root, less its extension.
nameOf :: FilePath -> FilePath -> Text.Text
nameOf root path =
  let beneath = Text.pack (drop (length root + 1) path)
   in Maybe.fromMaybe beneath (Text.stripSuffix (Text.pack ".json") beneath)

-- | The scenario named (a file name beneath data/scenarios, less its
-- extension), or why it does not decode.
decode :: Text.Text -> IO (Either String Scenario.Type.Scenario)
decode name = do
  root <- Load.defaultRoot
  (_, decoded) <- Load.loadFile (root <> "/" <> Text.unpack name <> ".json")
  pure (either (Left . Text.unpack) Right decoded)

-- | Where the replay was left for.
data Exit
  = MkToList
  | -- | Plays on from this position, each seat filled so
    -- (Web.Replay.MkBranch).
    MkPlayOn Natural (Map.Map PlayerId.PlayerId Web.Replay.Filling)

-- | The scenario at a position as a seat sees it (the player's of a game
-- finished here, `played`, else the first), replayed afresh at each
-- position the page goes to, each told to `moved`, until the page leaves
-- it, stepping on by itself while played, until the end; a game finished
-- here names each seat by who played it (History.seatNames), and says how
-- it ended at the end; a scenario names each by its number, and its lines
-- name each as the table does (Web.Replay.tableNames). The state at each
-- position a turn begins is kept once reached, for the log of every later
-- position (Web.Replay.earlier), and what each run keeps for the next to
-- resume from (Scenario.replay).
-- While no tap waits, the runs go on ahead a position at a time, so a
-- later position is kept before it is asked for; a step's taps
-- queued while the engine works add up to one move. How each seat is to be
-- filled to play on is kept as its menus choose (Web.Replay.refilled).
viewer :: Browser.Page Screen.Event -> (Natural -> IO ()) -> Registry.Registry Identity.Identity -> Text.Text -> Maybe History.Game -> Scenario.Type.Scenario -> Natural -> IO Exit
viewer page moved registry name played scenario given = do
  kept <- IORef.newIORef Map.empty
  resumable <- IORef.newIORef Scenario.keeping
  furthest <- IORef.newIORef 0
  let count = Natural.length (Scenario.Type.timeline scenario)
      labelled = seatsOf scenario
      names = foldMap History.seatNames played
      seats = Web.Replay.seatsNamed names labelled
      -- A game finished here names its seats by who played them; a
      -- scenario's lines name them as the table does to the seat viewed as.
      lineNames viewing = if Maybe.isJust played then names else Web.Replay.tableNames viewing labelled
      seen = played >>= History.viewerLabel
      lookUp = Identity.runIdentity . Interpreter.chosenCard registry
      starts = Web.Replay.turnStarts (Foldable.toList (Scenario.Type.timeline scenario))
      outcome = foldMap (Text.unwords . History.said) played
      -- The run to a position, its state kept where a turn begins, resumed
      -- from what the runs before it kept.
      run at = do
        before <- IORef.readIORef resumable
        (after, stop) <- Exception.evaluate (Identity.runIdentity $ Scenario.replay registry before (Just at) scenario)
        IORef.writeIORef resumable after
        Monad.when (at `elem` starts) (either (const (pure ())) (IORef.modifyIORef' kept . Map.insert at . Stop.state) stop)
        -- A failure fails every later position too: none is left to run.
        IORef.modifyIORef' furthest (max (either (const count) (const at) stop))
        pure stop
      reach at = moved at >> run at
      -- The run a few positions past the furthest any reached, no further
      -- than the next turn's start (which it keeps), if one is left:
      -- whether it ran. Each run resumes at a whole step's start, so one a
      -- position at a time would run a step's first entries again for each.
      ahead = do
        reached <- IORef.readIORef furthest
        if reached >= count then pure False else True <$ run (min (reached + 8) (Web.Replay.stepTo starts count reached Web.Replay.MkTurnOn))
      -- The next tap, the runs going on ahead while none waits.
      awaiting = do
        waiting <- Browser.pending page
        ran <- if waiting then pure False else ahead
        if ran then Browser.pause >> awaiting else Browser.next page
      stepOf event = case event of
        Screen.MkReplay (Web.Replay.MkStep way) -> Just way
        _ -> Nothing
      -- The states at the positions before this one where a turn begins.
      earlierOf at = fmap Maybe.catMaybes . Monad.forM (filter (< at) starts) $ \s -> do
        known <- Map.lookup s <$> IORef.readIORef kept
        maybe (fmap (either (const Nothing) (Just . Stop.state)) (run s)) (pure . Just) known
      go playing at stop viewing reading fills = do
        earlier <- earlierOf at
        let frame = (frameOf lookUp (lineNames viewing) name scenario seats count playing at stop viewing reading) {Web.Replay.earlier = fmap (PlayerView.Engine.viewFor viewing) earlier, Web.Replay.outcome = outcome, Web.Replay.kept = fmap History.key played, Web.Replay.chosenFillings = fills}
        Browser.display page . fmap Screen.MkReplay $ Web.Replay.view frame
        (tapped, value) <- awaiting
        let moveTo target = if target == at then go playing at stop viewing Nothing fills else reach target >>= \next -> go (playing && target < count) target next viewing Nothing fills
            readingOf object pinned = do
              board <- Web.Replay.board frame
              o <- Wording.recalled board object
              pure (Web.Card.MkReading object (either (pure . Text.pack . Exception.displayException) id (Web.Card.oracle lookUp o)) pinned)
            held = maybe False Web.Card.pinned reading
        case tapped of
          Screen.MkReplay event -> case event of
            Web.Replay.MkStep way -> do
              queued <- Browser.takeQueued page stepOf
              moveTo (Foldable.foldl' (Web.Replay.stepTo starts count) at (way : queued))
            Web.Replay.MkScrub -> maybe (go playing at stop viewing reading fills) (moveTo . min count) (Read.readMaybe (Text.unpack value))
            Web.Replay.MkPlay on -> go (on && at < count) at stop viewing reading fills
            Web.Replay.MkSeat pid -> go playing at stop pid reading fills
            Web.Replay.MkTable (Web.Question.MkRead object) -> go playing at stop viewing (readingOf object True <|> reading) fills
            Web.Replay.MkTable (Web.Question.MkPeek object) | not held -> go playing at stop viewing (readingOf object False <|> reading) fills
            Web.Replay.MkTable Web.Question.MkUnpeek | not held -> go playing at stop viewing Nothing fills
            Web.Replay.MkTable Web.Question.MkClose -> go playing at stop viewing Nothing fills
            Web.Replay.MkDownload k -> mapM_ (downloadPlayed k) played >> go playing at stop viewing reading fills
            Web.Replay.MkBranch filled -> pure (MkPlayOn at filled)
            Web.Replay.MkBack -> pure MkToList
            -- A seat's filling, or its bot's settings, chosen.
            _ -> go playing at stop viewing reading (Web.Replay.refilled event value frame)
          _ -> go playing at stop viewing reading fills
      first = min count given
  stop <- reach first
  go False first stop (maybe (PlayerId.MkPlayerId 0) fst (Maybe.listToMaybe (filter ((`elem` seen) . snd) labelled <> labelled))) Nothing Map.empty

-- | Saves the scenario file of the game finished here of this key to the
-- player's device, named by when it ended (History.fileName).
downloadPlayed :: Text.Text -> History.Game -> IO ()
downloadPlayed k game = Browser.download (History.fileName game) =<< Browser.playedScenario k

-- | Plays on from a position of the scenario: the run takes the timeline
-- to that stop (Scenario.branch), then each seat is answered as `filled`
-- fills it (Web.Replay.Filling), a seat absent at this screen: there, on
-- the page as the hotseat answers one (Hotseat.exchange); by the search
-- bot at its difficulty and persona (Lobby.botPlay), dealing what it cannot
-- see from the scenario's format's pool among `cards` by the legality table
-- `table` gives (Command.pooled), the page live while it decides
-- (Hotseat.breathing); or by the goldfish (Game.goldfish). Chance comes
-- from a fresh key, the page plays at the auto-pass level kept in
-- `levelRef`, until the game ends; how it ended, told to the seat at this
-- screen where it is the only one, why the bots could not deal, or
-- nothing when it was given up from the home screen (`leave`). Each bot
-- remembers what the game's log shows it, from the start
-- (Search.remembering).
playOn :: Browser.Page Screen.Event -> Registry.Registry Identity.Identity -> [Either Text.Text Card.Card] -> IO (Either Text.Text LegalityTable.LegalityTable) -> IORef.IORef Web.Question.Level -> Map.Map PlayerId.PlayerId Web.Replay.Filling -> Scenario.Type.Scenario -> Natural -> (Maybe PlayerView.PlayerView -> IO Hotseat.Leave) -> IO (Maybe [Text.Text])
playOn page registry cards table levelRef filled scenario given leave = do
  let numbered = seatsOf scenario
      fillingOf pid = Map.findWithDefault Web.Replay.MkHere pid filled
      mine = [pid | (pid, _) <- numbered, fillingOf pid == Web.Replay.MkHere]
      you = Maybe.fromMaybe Hotseat.player (Maybe.listToMaybe mine)
      reader = case mine of
        [only] -> Just only
        _ -> Nothing
      options = PlayOptions.none {PlayOptions.bots = Map.fromList [(PlayerId.unwrap pid + 1, Lobby.botPlay (level, style)) | (pid, _) <- numbered, Web.Replay.MkBot level style <- [fillingOf pid]]}
      bot seat = Map.member (PlayerId.unwrap seat + 1) (PlayOptions.bots options)
      format = case Scenario.Type.start scenario of
        Start.FromDecks settings _ _ -> GameSettings.format settings
        Start.FromBoard _ -> Nothing
  dealt <- if Map.null (PlayOptions.bots options) then pure (Right Nothing) else fmap Just <$> Command.pooled table format cards
  case dealt of
    Left problems -> pure (Just (Text.pack "The bots could not play:" : problems))
    Right prior -> do
      seatRef <- IORef.newIORef . Hotseat.newSeat you Ending.unseen =<< IORef.readIORef levelRef
      Hotseat.keepingLevel levelRef seatRef . Exception.handle (\Hotseat.MkAbandonedGame -> pure Nothing) $ do
        key <- Key.fresh
        memories <- IORef.newIORef Map.empty
        aims <- IORef.newIORef Map.empty
        breathRef <- IORef.newIORef Nothing
        botReading <- IORef.newIORef Nothing
        let at = min (Natural.length (Scenario.Type.timeline scenario)) given
            reference = Registry.hoist (pure . Identity.runIdentity) registry :: Registry.Registry IO
            lookUp = Identity.runIdentity . Interpreter.chosenCard registry
            here = Hotseat.exchange lookUp page seatRef leave
            breathe = Hotseat.breathing lookUp page seatRef leave botReading breathRef
            answererOf pid = case (fillingOf pid, prior) of
              (Web.Replay.MkGoldfish, _) -> Game.goldfish reference
              (_, Nothing) -> Game.wired reference here
              (_, Just p) -> Command.answerer reference here (pure Nothing) (const (pure ())) (IORef.readIORef memories) (Search.keptBy (IORef.readIORef aims) (IORef.modifyIORef' aims) pid) breathe (Command.timed (PlayOptions.thinkLimit options)) options p (PlayerId.unwrap pid + 1)
            seats = Game.consulting reference (Map.fromList [(pid, answererOf pid) | (pid, _) <- numbered])
            observers = [Search.remembering (IORef.modifyIORef' memories), Link.deciding (Hotseat.moving seatRef breathRef botReading mine bot)]
            onward = Answerer.MkAnswerer (Driver.ask observers seats)
        outcome <- Game.playing (Generator.fromKey key) (Scenario.branch (Game.reference reference) at onward scenario)
        pure . Just $ case outcome of
          Left reason -> [Text.pack "The game stopped: " <> reason <> Text.pack "."]
          Right (Left failure) -> [Text.pack "The scenario failed before the position:", Scenario.render failure]
          Right (Right gs) -> maybe [Text.pack "The game ran out of steps."] (\result -> Wording.result reader result : Wording.departures reader gs) (GameState.result gs)

-- | Each seat and the name the scenario gives it, in seating order.
seatsOf :: Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)]
seatsOf scenario =
  let labels = case Scenario.Type.start scenario of
        Start.FromBoard board -> fmap Seat.name (NonEmpty.toList (Board.seats board))
        Start.FromDecks _ entrants _ -> fmap Entrant.name (NonEmpty.toList entrants)
   in zip (fmap PlayerId.MkPlayerId [0 ..]) (fmap Label.unwrap labels)

-- | What the page shows of a stop, each seat named in its entry's line as
-- `names` names it by its label, stepping on by itself where `playing`.
frameOf :: (CardName.CardName -> Either e (Maybe Card.Card)) -> Map.Map Text.Text Text.Text -> Text.Text -> Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)] -> Natural -> Bool -> Natural -> Either ScenarioFailure.ScenarioFailure Stop.Stop -> PlayerId.PlayerId -> Maybe Web.Card.Reading -> Web.Replay.Frame
frameOf lookUp names name scenario seats count playing at stop seen reading =
  let base = Web.Replay.MkFrame name (Scenario.Type.description scenario) seats seen at count (Web.Replay.turnStarts (Foldable.toList (Scenario.Type.timeline scenario))) playing Nothing [] Text.empty 0 Nothing Nothing Nothing reading Map.empty Nothing Map.empty
      shown board = base {Web.Replay.board = Just board, Web.Replay.written = Web.Card.written lookUp board}
   in case stop of
        Left failure -> base {Web.Replay.failure = Just (Scenario.render failure)}
        Right reached@(Stop.MkBefore _ timed asked) ->
          let board = PlayerView.Engine.viewFor seen (Stop.state reached)
           in (shown board)
                { Web.Replay.depth = Protocol.depthOf asked,
                  Web.Replay.entryLine = Just (Web.Replay.lineOf names (PlayerView.begun board) (Protocol.requestFor asked) timed),
                  Web.Replay.question = Protocol.requestFor asked
                }
        Right reached@(Stop.MkEnded _) -> shown (PlayerView.Engine.viewFor seen (Stop.state reached))
