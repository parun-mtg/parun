-- | A scenario replayed on the page (#8821): each position reached by
-- replaying the timeline from the board with the test runner's own run
-- (Parun.Scenario.replay), shown as a chosen seat sees it
-- (Parun.Web.Replay). With no scenario named, the list of them.
module Parun.Browser.Replay where

import Control.Applicative ((<|>))
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Clock as Clock
import Numeric.Natural (Natural)
import qualified Parun.Browser as Browser
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Load as Load
import qualified Parun.Types.Board as Board
import qualified Parun.Types.Entrant as Entrant
import qualified Parun.Types.Label as Label
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Scenario as Scenario.Type
import qualified Parun.Types.ScenarioFailure as ScenarioFailure
import qualified Parun.Types.Seat as Seat
import qualified Parun.Types.Start as Start
import qualified Parun.Types.Stop as Stop
import qualified Parun.Types.Timed as Timed
import qualified Parun.Web.Card as Web.Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Replay as Web.Replay
import qualified System.Environment as Environment
import qualified Text.Read as Read

-- | The scenario named (a file name beneath data/scenarios, less its
-- extension), or the list of them for the empty name, on a page mounted
-- first so an exception that escapes is shown there.
start :: String -> IO ()
start named = do
  page <- Browser.mount
  Exception.catch (if null named then choose page else replay page (Text.pack named)) $ \problem ->
    Browser.display page $ message ["The replay stopped on an error:", Exception.displayException (problem :: Exception.SomeException)]

-- | The list of scenarios, filtered as typed.
choose :: Browser.Page Web.Replay.Event -> IO ()
choose page = do
  root <- Load.defaultRoot
  paths <- Load.listRoot root
  let names = fmap (nameOf root) paths
      go typed = do
        Browser.display page $ Web.Replay.picker typed names
        (event, value) <- Browser.next page
        go (if event == Web.Replay.MkSearch then value else typed)
  go Text.empty

-- | A path's name beneath the root, less its extension.
nameOf :: FilePath -> FilePath -> Text.Text
nameOf root path =
  let beneath = Text.pack (drop (length root + 1) path)
   in Maybe.fromMaybe beneath (Text.stripSuffix (Text.pack ".json") beneath)

-- | The scenario at a position (parun_at, else its start) as a seat sees it
-- (the first), replayed afresh at each position the page goes to.
replay :: Browser.Page Web.Replay.Event -> Text.Text -> IO ()
replay page name = do
  root <- Load.defaultRoot
  (_, decoded) <- Load.loadFile (root <> "/" <> Text.unpack name <> ".json")
  case decoded of
    Left problem -> Browser.display page $ message ["The scenario " <> Text.unpack name <> " does not decode:", Text.unpack problem]
    Right scenario -> do
      Browser.display page $ message ["Loading the cards\x2026"]
      cardRoot <- Registry.defaultRoot
      loaded <- fmap (Registry.loadBundle cardRoot) $ Registry.bundlePath >>= ByteString.readFile
      registry <- Registry.defaultRegistry loaded :: IO (Registry.Registry Identity.Identity)
      given <- (>>= Read.readMaybe) <$> Environment.lookupEnv "parun_at"
      let count = Natural.length (Scenario.Type.timeline scenario)
          seats = seatsOf scenario
          lookUp = Identity.runIdentity . Interpreter.chosenCard registry
          -- The run to a position, timed on the trace.
          reach at = do
            began <- Clock.getMonotonicTime
            stop <- Exception.evaluate . Identity.runIdentity $ Scenario.replay registry (Just at) scenario
            ended <- Clock.getMonotonicTime
            Browser.trace page . Text.pack $ "replay: position " <> show at <> " in " <> Browser.milliseconds (ended - began)
            pure stop
          go at stop viewer reading = do
            let frame = frameOf name scenario seats count at stop viewer reading
            Browser.display page $ Web.Replay.view frame
            (event, value) <- Browser.next page
            let moveTo target = if target == at then go at stop viewer Nothing else reach target >>= \next -> go target next viewer Nothing
                readingOf object pinned = do
                  board <- Web.Replay.board frame
                  o <- Map.lookup object (Render.objects board)
                  pure (Web.Card.MkReading object (either (pure . Text.pack . Exception.displayException) id (Web.Card.oracle lookUp o)) pinned)
                held = maybe False Web.Card.pinned reading
            case event of
              Web.Replay.MkGo target -> moveTo (min count target)
              Web.Replay.MkScrub -> maybe (go at stop viewer reading) (moveTo . min count) (Read.readMaybe (Text.unpack value))
              Web.Replay.MkSeat pid -> go at stop pid reading
              Web.Replay.MkTable (Web.Question.MkRead object) -> go at stop viewer (readingOf object True <|> reading)
              Web.Replay.MkTable (Web.Question.MkPeek object) | not held -> go at stop viewer (readingOf object False <|> reading)
              Web.Replay.MkTable Web.Question.MkUnpeek | not held -> go at stop viewer Nothing
              Web.Replay.MkTable Web.Question.MkClose -> go at stop viewer Nothing
              _ -> go at stop viewer reading
          first = min count (Maybe.fromMaybe 0 given)
      stop <- reach first
      go first stop (maybe (PlayerId.MkPlayerId 0) fst (Maybe.listToMaybe seats)) Nothing

-- | Each seat and the name the scenario gives it, in turn order.
seatsOf :: Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)]
seatsOf scenario =
  let labels = case Scenario.Type.start scenario of
        Start.FromBoard board -> fmap Seat.name (NonEmpty.toList (Board.seats board))
        Start.FromDecks _ entrants -> fmap Entrant.name (NonEmpty.toList entrants)
   in zip (fmap PlayerId.MkPlayerId [0 ..]) (fmap Label.unwrap labels)

-- | What the page shows of a stop.
frameOf :: Text.Text -> Scenario.Type.Scenario -> [(PlayerId.PlayerId, Text.Text)] -> Natural -> Natural -> Either ScenarioFailure.ScenarioFailure Stop.Stop -> PlayerId.PlayerId -> Maybe Web.Card.Reading -> Web.Replay.Frame
frameOf name scenario seats count at stop viewer reading =
  let base = Web.Replay.MkFrame name (Scenario.Type.description scenario) seats viewer at count Nothing Nothing Nothing Nothing reading
   in case stop of
        Left failure -> base {Web.Replay.failure = Just (Scenario.render failure)}
        Right reached@(Stop.MkBefore _ timed asked) ->
          base
            { Web.Replay.board = Just (PlayerView.Engine.viewFor viewer (Stop.state reached)),
              Web.Replay.entry = Just (Scenario.renderWhen (Timed.when timed) <> Text.pack ": " <> Scenario.renderEntry (Timed.entry timed)),
              Web.Replay.question = Protocol.requestFor asked
            }
        Right reached@(Stop.MkEnded _) -> base {Web.Replay.board = Just (PlayerView.Engine.viewFor viewer (Stop.state reached))}

message :: [String] -> Html.Html msg
message said = Element.el "main" ["message"] [] [Element.el "p" [] [] [Element.string t] | t <- said]
