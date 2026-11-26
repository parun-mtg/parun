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
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.DeckList as DeckList
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Load as Load
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.Label as Label
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.ScenarioFailure as ScenarioFailure
import qualified Parun.Types.Start as Start
import qualified Parun.Types.Stop as Stop
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.History as History
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
  | -- | Plays on from this position, the host at the seat viewed as
    -- (Web.Replay.MkBranch).
    MkPlayOn Natural PlayerId.PlayerId

-- | The scenario at a position as a seat sees it (the player's of a game
-- finished here, `played`, else the first), replayed afresh at each
-- position the page goes to, each told to `moved`, until the page leaves
-- it, stepping on by itself while played, until the end; a game finished
-- here names each seat by who played it (History.seatNames), and says how
-- it ended at the end; a scenario names each by its number, and its lines
-- name each as the table does (Web.Replay.tableNames). The state at each
-- position a turn begins is kept once reached, and the log each seat read
-- through it once read, for the log of every later position
-- (Web.Replay.earlier); so is what each run keeps for the next to resume
-- from (Scenario.replay).
-- While no tap waits, the runs go on ahead a few positions at a time, so a
-- later position is kept before it is asked for; a step's taps
-- queued while the engine works add up to one move.
viewer :: Browser.Page Screen.Event -> (Natural -> IO ()) -> Registry.Registry Identity.Identity -> Text.Text -> Maybe History.Game -> Scenario.Type.Scenario -> Natural -> IO Exit
viewer page moved registry name played scenario given = do
  kept <- IORef.newIORef Map.empty
  logs <- IORef.newIORef Map.empty
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
      -- The turns' starts before it run first, so the run to it is the
      -- one paused there, which the next step goes on from.
      reach at = moved at >> mapM_ stateAt (filter (< at) starts) >> run at
      -- The run a few positions past the furthest any reached, no further
      -- than the next turn's start (which it keeps), if one is left:
      -- whether it ran. Each run gives the page a turn after it, so runs
      -- of a few positions keep a tap's wait short at little cost.
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
      -- The state at a position where a turn begins, unless the run failed first.
      stateAt s = do
        known <- Map.lookup s <$> IORef.readIORef kept
        maybe (fmap (either (const Nothing) (Just . Stop.state)) (run s)) (pure . Just) known
      -- The log as the seat read it at the positions before this one where
      -- a turn begins, each turn's read once.
      earlierOf viewing at = Monad.foldM (readTo viewing) Web.Replay.unread (filter (< at) starts)
      readTo viewing logged s = do
        known <- Map.lookup (viewing, s) <$> IORef.readIORef logs
        case known of
          Just through -> pure through
          Nothing -> do
            state <- stateAt s
            let through = maybe logged (Web.Replay.readOn logged . pure . PlayerView.Engine.viewFor viewing) state
            through <$ IORef.modifyIORef' logs (Map.insert (viewing, s) through)
      go playing at stop viewing reading = do
        earlier <- earlierOf viewing at
        let frame = (frameOf lookUp (lineNames viewing) name scenario seats count playing at stop viewing reading) {Web.Replay.earlier = earlier, Web.Replay.outcome = outcome, Web.Replay.kept = fmap History.key played}
        Browser.display page . fmap Screen.MkReplay $ Web.Replay.view frame
        (tapped, value) <- awaiting
        let moveTo target = if target == at then go playing at stop viewing Nothing else reach target >>= \next -> go (playing && target < count) target next viewing Nothing
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
            Web.Replay.MkScrub -> maybe (go playing at stop viewing reading) (moveTo . min count) (Read.readMaybe (Text.unpack value))
            Web.Replay.MkPlay on -> go (on && at < count) at stop viewing reading
            Web.Replay.MkSeat pid -> go playing at stop pid reading
            Web.Replay.MkTable (Web.Question.MkRead object) -> go playing at stop viewing (readingOf object True <|> reading)
            Web.Replay.MkTable (Web.Question.MkPeek object) | not held -> go playing at stop viewing (readingOf object False <|> reading)
            Web.Replay.MkTable Web.Question.MkUnpeek | not held -> go playing at stop viewing Nothing
            Web.Replay.MkTable Web.Question.MkClose -> go playing at stop viewing Nothing
            Web.Replay.MkDownload k -> mapM_ (downloadPlayed k) played >> go playing at stop viewing reading
            Web.Replay.MkBranch -> pure (MkPlayOn at viewing)
            Web.Replay.MkBack -> pure MkToList
            _ -> go playing at stop viewing reading
          _ -> go playing at stop viewing reading
      first = min count given
  stop <- reach first
  go False first stop (maybe (PlayerId.MkPlayerId 0) fst (Maybe.listToMaybe (filter ((`elem` seen) . snd) labelled <> labelled))) Nothing

-- | Saves the scenario file of the game finished here of this key to the
-- player's device, named by when it ended (History.fileName).
downloadPlayed :: Text.Text -> History.Game -> IO ()
downloadPlayed k game = Browser.download (History.fileName game) =<< Browser.playedScenario k

-- | Each seat's deck as a replay names it, in seating order: a game
-- finished here's as its match-up did (History.deck), a scenario's by the
-- name its list gives it (DeckList.nameOf), else by the seat's label.
placesOf :: Maybe History.Game -> Scenario.Type.Scenario -> [Text.Text]
placesOf played scenario = case (played, Scenario.Type.start scenario) of
  (Just game, _) | length (History.seats game) == length labels -> fmap History.deck (History.seats game)
  (_, Start.FromDecks _ entrants _) -> [Maybe.fromMaybe (Label.unwrap (Entrant.name entrant)) (DeckList.nameOf (Text.unlines (Entrant.deck entrant))) | entrant <- Foldable.toList entrants]
  _ -> fmap (\label -> Label.unwrap label <> Text.pack ", as the board stands") labels
  where
    labels = Scenario.seatsOf scenario

-- | Each seat and the name the scenario gives it, in seating order.
seatsOf :: Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)]
seatsOf = zip (fmap PlayerId.MkPlayerId [0 ..]) . fmap Label.unwrap . Scenario.seatsOf

-- | What the page shows of a stop, each seat named in its entry's line as
-- `names` names it by its label, stepping on by itself where `playing`.
frameOf :: (CardName.CardName -> Either e (Maybe Card.Card)) -> Map.Map Text.Text Text.Text -> Text.Text -> Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)] -> Natural -> Bool -> Natural -> Either ScenarioFailure.ScenarioFailure Stop.Stop -> PlayerId.PlayerId -> Maybe Web.Card.Reading -> Web.Replay.Frame
frameOf lookUp names name scenario seats count playing at stop seen reading =
  let base = Web.Replay.MkFrame name (Scenario.Type.description scenario) seats seen at count (Web.Replay.turnStarts (Foldable.toList (Scenario.Type.timeline scenario))) playing Nothing Web.Replay.unread Text.empty 0 Nothing Nothing Nothing reading Map.empty Nothing
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
