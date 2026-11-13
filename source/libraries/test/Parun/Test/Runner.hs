-- | A test runner on boot libraries alone. It collects the cases a
-- 'Spec.Spec' registers, shuffles them, runs them on a pool of one worker per
-- capability, prints each failure as it lands, and ends with one summary line.
module Parun.Test.Runner where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Word as Word
import qualified GHC.Clock as Clock
import qualified GHC.Stack as Stack
import Numeric.Natural (Natural)
import qualified Parun.Exceptions.AssertionFailed as AssertionFailed
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Generator as Generator
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Types.Key as Key.Type
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.Timeout as Timeout
import qualified Text.Read as Read

-- | One registered case: its group names, outermost first, then its own.
data Case = MkCase
  { path :: [Text.Text],
    action :: IO ()
  }

-- | What a case came to; a failure carries its message.
data Outcome
  = Passed
  | Failed Text.Text
  deriving (Eq, Show)

-- | The command line: the substrings selecting cases (any of them; every case
-- when none), the per-case budget in microseconds, the shuffle's seed, and how
-- many of the slowest cases to name.
data Options = MkOptions
  { patterns :: [Text.Text],
    timeout :: Maybe Natural,
    seed :: Maybe Key.Type.Key,
    slowest :: Natural
  }
  deriving (Eq, Show)

-- | How many cases ran, and how many of them failed.
data Summary = MkSummary
  { total :: Natural,
    failures :: Natural
  }
  deriving (Eq, Show)

-- | Registers each case under the groups around it; a failed assertion throws
-- 'AssertionFailed.AssertionFailed'.
runner :: (Monad n) => Spec.Spec IO (Writer.WriterT (Seq.Seq Case) n)
runner =
  Spec.MkSpec
    { Spec.assertFailure = \m -> Exception.throwIO . AssertionFailed.MkAssertionFailed (Text.pack m) $ Stack.getCallStack Stack.callStack,
      Spec.describe = \s -> Writer.censor (fmap (\c -> c {path = Text.pack s : path c})),
      Spec.it = \s a -> Writer.tell . Seq.singleton $ MkCase {path = [Text.pack s], action = a}
    }

-- | Parses the command line, collects the cases, runs the selected ones and
-- exits with the outcome.
main :: IO (Seq.Seq Case) -> IO ()
main collect = do
  arguments <- Environment.getArgs
  options <- case parseOptions arguments of
    Left problem -> do
      IO.hPutStrLn IO.stderr problem
      IO.hPutStr IO.stderr $ GetOpt.usageInfo "usage: parun test [OPTION...]" flags
      Exit.exitFailure
    Right o -> pure o
  IO.hSetBuffering IO.stdout IO.LineBuffering
  s <- maybe Key.fresh pure $ seed options
  cases <- collect
  lock <- Concurrent.newMVar ()
  let report line = Concurrent.withMVar lock . const $ TextIO.putStrLn line
  summary <- run report options s cases
  TextIO.putStrLn $ summaryLine s summary
  Monad.unless (succeeded summary) Exit.exitFailure

flags :: [GetOpt.OptDescr (Options -> Either String Options)]
flags =
  [ GetOpt.Option "p" ["pattern"] (GetOpt.ReqArg (\s o -> Right o {patterns = patterns o <> [Text.pack s]}) "SUBSTRING") "run the cases whose path holds this; repeated, those matching any",
    GetOpt.Option [] ["timeout"] (GetOpt.ReqArg (\s o -> maybe (Left $ "not a duration: " <> s) (\n -> Right o {timeout = Just n}) $ duration s) "DURATION") "fail a case still running after this long: seconds, or a number ending in ms, s, m or h",
    GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left $ "not a seed: " <> s) (\k -> Right o {seed = Just k}) . Key.fromHex $ Text.pack s) "KEY") "the shuffle's key, 64 hexadecimal digits; drawn from the operating system when absent",
    GetOpt.Option [] ["slowest"] (GetOpt.ReqArg (\s o -> maybe (Left $ "not a number of cases: " <> s) (\n -> Right o {slowest = n}) $ Read.readMaybe s) "N") "name the N cases that ran longest, and for how long, before the summary"
  ]

parseOptions :: [String] -> Either String Options
parseOptions arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [], []) -> Monad.foldM (\o f -> f o) MkOptions {patterns = [], timeout = Nothing, seed = Nothing, slowest = 0} updates
  (_, argument : _, []) -> Left $ "unexpected argument: " <> argument
  (_, _, problem : _) -> Left $ List.dropWhileEnd Char.isSpace problem

-- | A duration in microseconds: a count of seconds, or of the unit it ends in.
duration :: String -> Maybe Natural
duration s = do
  let (digits, unit) = span Char.isDigit s
  n <- Read.readMaybe digits
  scale <- case unit of
    "" -> Just 1000000
    "ms" -> Just 1000
    "s" -> Just 1000000
    "m" -> Just 60000000
    "h" -> Just 3600000000
    _ -> Nothing
  pure $ n * scale

-- | A case's path as a pattern sees it: its names joined by dots.
name :: Case -> Text.Text
name c = Text.intercalate (Text.pack ".") $ path c

selected :: Options -> Case -> Bool
selected options c = null (patterns options) || any (\p -> Text.isInfixOf p $ name c) (patterns options)

-- | Runs the selected cases in an order shuffled by the seed, handing each
-- failure's line to @report@ as it lands, then the slowest cases' lines.
run :: (Text.Text -> IO ()) -> Options -> Key.Type.Key -> Seq.Seq Case -> IO Summary
run report options s cases = do
  let order = ordered options s cases
  queue <- IORef.newIORef order
  failed <- IORef.newIORef (0 :: Natural)
  durations <- IORef.newIORef []
  let worker = do
        next <- IORef.atomicModifyIORef' queue $ \q -> case q of
          [] -> ([], Nothing)
          c : rest -> (rest, Just c)
        case next of
          Nothing -> pure ()
          Just c -> do
            started <- Clock.getMonotonicTimeNSec
            outcome <- runCase (timeout options) c
            finished <- Clock.getMonotonicTimeNSec
            IORef.atomicModifyIORef' durations $ \ds -> (((finished - started) `div` 1000000, name c) : ds, ())
            case outcome of
              Passed -> pure ()
              Failed m -> do
                IORef.atomicModifyIORef' failed $ \n -> (n + 1, ())
                report $ failureLine c m
            worker
  workers <- Concurrent.getNumCapabilities
  dones <- Monad.replicateM workers $ do
    done <- Concurrent.newEmptyMVar
    Monad.void . Concurrent.forkIO $ Exception.finally worker (Concurrent.putMVar done ())
    pure done
  Foldable.traverse_ Concurrent.takeMVar dones
  n <- IORef.readIORef failed
  IORef.readIORef durations >>= Foldable.traverse_ report . slowestLines (slowest options)
  pure MkSummary {total = Natural.length order, failures = n}

-- | A line for each of the @n@ cases that ran longest, longest first: its
-- milliseconds, then its path.
slowestLines :: Natural -> [(Word.Word64, Text.Text)] -> [Text.Text]
slowestLines n = fmap (\(ms, path') -> Text.pack ("SLOW " <> show ms <> "ms ") <> path') . take (Natural.toIntSaturating n) . List.sortOn (Bifunctor.first Ord.Down)

-- | The selected cases, in the order the seed shuffles them into.
ordered :: Options -> Key.Type.Key -> Seq.Seq Case -> [Case]
ordered options s cases = State.evalState (Generator.shuffle . filter (selected options) $ Foldable.toList cases) $ Generator.fromKey s

-- | Runs one case under the budget, if there is one. Any exception fails it
-- with its own message, not the context GHC wraps it in, and so does a message
-- that throws while it is rendered.
runCase :: Maybe Natural -> Case -> IO Outcome
runCase limit c = do
  result <- Exception.try $ case limit of
    Nothing -> fmap Just $ action c
    Just us -> Timeout.timeout (Natural.toIntSaturating us) $ action c
  case result of
    Right (Just ()) -> pure Passed
    Right Nothing -> pure . Failed $ Text.pack "timed out"
    Left (Exception.SomeException e) -> do
      rendered <- Exception.try . Exception.evaluate . Text.pack $ Exception.displayException e
      pure . Failed $ case rendered of
        Right m -> m
        Left problem -> unrendered problem

unrendered :: Exception.SomeException -> Text.Text
unrendered _ = Text.pack "threw an exception whose message also threw"

-- | A failure's path and message, the message's later lines indented.
failureLine :: Case -> Text.Text -> Text.Text
failureLine c m = Text.intercalate (Text.pack "\n  ") $ Text.lines (Text.pack "FAIL " <> name c <> Text.pack ": " <> m)

succeeded :: Summary -> Bool
succeeded summary = total summary > 0 && failures summary == 0

-- | The last line of a run. An empty selection is neither a pass nor a
-- failure, so it reads differently from both.
summaryLine :: Key.Type.Key -> Summary -> Text.Text
summaryLine s summary
  | total summary == 0 = Text.pack "Matched no tests with seed " <> Key.toHex s <> Text.pack "."
  | failures summary == 0 = Text.pack ("Passed all " <> show (total summary) <> " tests with seed ") <> Key.toHex s <> Text.pack "."
  | otherwise = Text.pack ("Failed " <> show (failures summary) <> " of " <> show (total summary) <> " tests with seed ") <> Key.toHex s <> Text.pack "."
