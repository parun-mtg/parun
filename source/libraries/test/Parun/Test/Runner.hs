{-# LANGUAGE ApplicativeDo #-}

-- | A test runner on boot libraries alone. It collects the cases a
-- 'Spec.Spec' registers, shuffles them, runs them on a pool of one worker per
-- capability, prints each failure as it lands, and ends with one summary line.
-- At a terminal it also keeps a line counting the cases run, redrawn in place
-- beneath the failures, and under GitHub Actions it also writes each failure as
-- an error annotation.
module Parun.Test.Runner where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified Data.Word as Word
import qualified GHC.Clock as Clock
import qualified GHC.Stack as Stack
import Numeric.Natural (Natural)
import qualified Parun.Exceptions.AssertionFailed as AssertionFailed
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Extra.Word64 as Word64
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
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

-- | What a case came to; a failure carries its message and, for an assertion,
-- where it was made.
data Outcome
  = Passed
  | Failed Text.Text (Maybe Site)
  deriving (Eq, Show)

-- | Where an assertion was made: its file, as the build names it, and line.
data Site = MkSite
  { sourceFile :: Text.Text,
    sourceLine :: Natural
  }
  deriving (Eq, Show)

-- | The command line: the substrings selecting cases (any of them; every case
-- when none), the per-case budget in microseconds, the shuffle's seed, and
-- where to write the run's 'Report'.
data Options = MkOptions
  { patterns :: [Text.Text],
    timeout :: Maybe Natural,
    seed :: Maybe Key.Type.Key,
    json :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | One case as it ran: its dotted path, why it failed if it did and where,
-- and how long it took.
data Result = MkResult
  { title :: Text.Text,
    failure :: Maybe Text.Text,
    site :: Maybe Site,
    nanoseconds :: Natural
  }
  deriving (Eq, Show)

-- | A whole run for a tool to read: the seed that ordered it, in hexadecimal,
-- and every case in the order it finished.
data Report = MkReport
  { shuffled :: Text.Text,
    results :: [Result]
  }
  deriving (Eq, Show)

siteCodec :: Codec.Codec Site
siteCodec = Fields.object $ do
  f <- Fields.required "file" Common.text sourceFile
  l <- Fields.required "line" Common.natural sourceLine
  pure MkSite {sourceFile = f, sourceLine = l}

resultCodec :: Codec.Codec Result
resultCodec = Fields.object $ do
  t <- Fields.required "name" Common.text title
  f <- Fields.defaulted "failure" Nothing (Common.maybe Common.text) failure
  w <- Fields.defaulted "site" Nothing (Common.maybe siteCodec) site
  n <- Fields.required "nanoseconds" Common.natural nanoseconds
  pure MkResult {title = t, failure = f, site = w, nanoseconds = n}

reportCodec :: Codec.Codec Report
reportCodec = Fields.object $ do
  k <- Fields.required "seed" Common.text shuffled
  rs <- Fields.required "cases" (Common.list resultCodec) results
  pure MkReport {shuffled = k, results = rs}

-- | How many cases ran, and how many of them failed.
data Summary = MkSummary
  { total :: Natural,
    failures :: Natural
  }
  deriving (Eq, Show)

-- | How failures and progress are shown: whether standard output is a
-- terminal, to keep a progress line on, and whether the run is under GitHub
-- Actions, to annotate.
data Display = MkDisplay
  { live :: Bool,
    annotated :: Bool
  }
  deriving (Eq, Show)

-- | What the terminal holds beneath the failures so far: the cases counted,
-- when the progress line was last drawn, and how wide it was.
data Screen = MkScreen
  { counted :: Summary,
    drawnAt :: Word.Word64,
    width :: Int
  }
  deriving (Eq, Show)

-- | Registers each case under the groups around it; a failed assertion throws
-- 'AssertionFailed.AssertionFailed', its call stack without the frames called
-- from inside "Parun.Spec", which every assertion shares.
runner :: (Monad n) => Spec.Spec IO (Writer.WriterT (Seq.Seq Case) n)
runner =
  Spec.MkSpec
    { Spec.assertFailure = \m -> Exception.throwIO . AssertionFailed.MkAssertionFailed (Text.pack m) . filter ((/= "Parun.Spec") . Stack.srcLocModule . snd) $ Stack.getCallStack Stack.callStack,
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
  terminal <- IO.hIsTerminalDevice IO.stdout
  actions <- Environment.lookupEnv "GITHUB_ACTIONS"
  let display = MkDisplay {live = terminal, annotated = actions == Just "true"}
  s <- maybe Key.fresh pure $ seed options
  cases <- collect
  let todo = ordered options s cases
  started <- Clock.getMonotonicTimeNSec
  screen <- IORef.newIORef MkScreen {counted = MkSummary {total = 0, failures = 0}, drawnAt = 0, width = 0}
  lock <- Concurrent.newMVar ()
  let write t = TextIO.putStr t >> IO.hFlush IO.stdout
  ran <- run (Concurrent.withMVar lock . const . shown write display (Natural.length todo) started screen) (timeout options) todo
  Monad.when (live display) $ IORef.readIORef screen >>= write . erased . width
  Foldable.for_ (json options) $ \file ->
    ByteString.writeFile file . Encoding.encodeUtf8 . Common.render $ Codec.encode reportCodec MkReport {shuffled = Key.toHex s, results = ran}
  let summary = summarize ran
  Foldable.for_ (rerunLine ran) TextIO.putStrLn
  TextIO.putStrLn $ summaryLine s summary
  Monad.unless (succeeded summary) Exit.exitFailure

flags :: [GetOpt.OptDescr (Options -> Either String Options)]
flags =
  [ GetOpt.Option "p" ["pattern"] (GetOpt.ReqArg (\s o -> Right o {patterns = patterns o <> [Text.pack s]}) "SUBSTRING") "run the cases whose path holds this; repeated, those matching any",
    GetOpt.Option [] ["timeout"] (GetOpt.ReqArg (\s o -> maybe (Left $ "not a duration: " <> s) (\n -> Right o {timeout = Just n}) $ duration s) "DURATION") "fail a case still running after this long: seconds, or a number ending in ms, s, m or h",
    GetOpt.Option [] ["seed"] (GetOpt.ReqArg (\s o -> maybe (Left $ "not a seed: " <> s) (\k -> Right o {seed = Just k}) . Key.fromHex $ Text.pack s) "KEY") "the shuffle's key, 64 hexadecimal digits; drawn from the operating system when absent",
    GetOpt.Option [] ["json"] (GetOpt.ReqArg (\file o -> Right o {json = Just file}) "FILE") "write every case's name, failure, its site and nanoseconds to this file as JSON"
  ]

parseOptions :: [String] -> Either String Options
parseOptions arguments = case GetOpt.getOpt GetOpt.Permute flags arguments of
  (updates, [], []) -> Monad.foldM (\o f -> f o) MkOptions {patterns = [], timeout = Nothing, seed = Nothing, json = Nothing} updates
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

-- | Runs the cases, each under the budget if there is one, handing each to
-- @report@ as it finishes, and answers every case as it ran, in the order they
-- finished.
run :: (Result -> IO ()) -> Maybe Natural -> [Case] -> IO [Result]
run report limit cases = do
  queue <- IORef.newIORef cases
  finished <- IORef.newIORef []
  let worker = do
        next <- IORef.atomicModifyIORef' queue $ \q -> case q of
          [] -> ([], Nothing)
          c : rest -> (rest, Just c)
        case next of
          Nothing -> pure ()
          Just c -> do
            before <- Clock.getMonotonicTimeNSec
            outcome <- runCase limit c
            after <- Clock.getMonotonicTimeNSec
            let (f, w) = failed outcome
                result = MkResult {title = name c, failure = f, site = w, nanoseconds = Word64.toNatural after - Word64.toNatural before}
            IORef.atomicModifyIORef' finished $ \rs -> (result : rs, ())
            report result
            worker
  workers <- Concurrent.getNumCapabilities
  dones <- Monad.replicateM workers $ do
    done <- Concurrent.newEmptyMVar
    Monad.void . Concurrent.forkIO $ Exception.finally worker (Concurrent.putMVar done ())
    pure done
  Foldable.traverse_ Concurrent.takeMVar dones
  fmap reverse $ IORef.readIORef finished

-- | A failure's message and site.
failed :: Outcome -> (Maybe Text.Text, Maybe Site)
failed outcome = case outcome of
  Passed -> (Nothing, Nothing)
  Failed m w -> (Just m, w)

summarize :: [Result] -> Summary
summarize ran = MkSummary {total = Natural.length ran, failures = Natural.length $ Maybe.mapMaybe failure ran}

-- | The selected cases, in the order the seed shuffles them into.
ordered :: Options -> Key.Type.Key -> Seq.Seq Case -> [Case]
ordered options s cases = State.evalState (Generator.shuffle . filter (selected options) $ Foldable.toList cases) $ Generator.fromKey s

-- | Runs one case under the budget, if there is one. Any exception fails it
-- with its own message, not the context GHC wraps it in, and so does a message
-- that throws while it is rendered; a failed assertion also says where it was
-- made.
runCase :: Maybe Natural -> Case -> IO Outcome
runCase limit c = do
  result <- Exception.try $ case limit of
    Nothing -> fmap Just $ action c
    Just us -> Timeout.timeout (Natural.toIntSaturating us) $ action c
  case result of
    Right (Just ()) -> pure Passed
    Right Nothing -> pure $ Failed (Text.pack "timed out") Nothing
    Left problem@(Exception.SomeException e) -> do
      rendered <- Exception.try . Exception.evaluate . Text.pack $ Exception.displayException e
      pure $ Failed (either unrendered id rendered) (siteOf =<< Exception.fromException problem)

-- | Where an assertion was made: the outermost frame of its call stack, which
-- is the case's own line unless a helper without a call stack stands between.
siteOf :: AssertionFailed.AssertionFailed -> Maybe Site
siteOf a = case reverse $ AssertionFailed.frames a of
  [] -> Nothing
  (_, loc) : _ -> Just MkSite {sourceFile = Text.replace (Text.pack "\\") (Text.pack "/") . Text.pack $ Stack.srcLocFile loc, sourceLine = Int.toNaturalSaturating $ Stack.srcLocStartLine loc}

unrendered :: Exception.SomeException -> Text.Text
unrendered _ = Text.pack "threw an exception whose message also threw"

-- | A failure's path and message, the message's later lines indented.
failureLine :: Text.Text -> Text.Text -> Text.Text
failureLine t m = Text.intercalate (Text.pack "\n  ") $ Text.lines (Text.pack "FAIL " <> t <> Text.pack ": " <> m)

-- | Shows a finished case, one of @count@, through @write@: its failure in
-- full, as an annotation too when 'annotated', and when 'live' the progress
-- line beneath, redrawn in place with a
-- carriage return after a failure, at the last case, and otherwise at most
-- every tenth of a second. The caller holds the lock.
shown :: (Text.Text -> IO ()) -> Display -> Natural -> Word.Word64 -> IORef.IORef Screen -> Result -> IO ()
shown write display count started screen result = do
  now <- Clock.getMonotonicTimeNSec
  before <- IORef.readIORef screen
  let tally = MkSummary {total = total (counted before) + 1, failures = failures (counted before) + maybe 0 (const 1) (failure result)}
      due = Maybe.isJust (failure result) || now - drawnAt before >= 100000000 || total tally == count
  Foldable.for_ (failure result) $ \m -> do
    Monad.when (live display) . write . erased $ width before
    write $ failureLine (title result) m <> Text.pack "\n"
    Monad.when (annotated display) . write $ annotation (title result) (site result) m <> Text.pack "\n"
  if live display && due
    then do
      let line = progressLine count tally . Word64.toNatural $ now - started
      write $ Text.pack "\r" <> Text.justifyLeft (width before) ' ' line
      IORef.writeIORef screen MkScreen {counted = tally, drawnAt = now, width = Text.length line}
    else IORef.writeIORef screen before {counted = tally}

-- | A failure as a GitHub Actions workflow command, which the run's page
-- shows as an error annotation, on its file and line when it has a site.
annotation :: Text.Text -> Maybe Site -> Text.Text -> Text.Text
annotation t w m =
  let located = foldMap (\x -> [Text.pack "file=" <> property (sourceFile x), Text.pack ("line=" <> show (sourceLine x))]) w
   in Text.pack "::error " <> Text.intercalate (Text.pack ",") (located <> [Text.pack "title=" <> property t]) <> Text.pack "::" <> escaped m
  where
    escaped = Text.replace (Text.pack "\n") (Text.pack "%0A") . Text.replace (Text.pack "\r") (Text.pack "%0D") . Text.replace (Text.pack "%") (Text.pack "%25")
    property = Text.replace (Text.pack ",") (Text.pack "%2C") . Text.replace (Text.pack ":") (Text.pack "%3A") . escaped

-- | Blanks a progress line this wide, leaving the cursor at its start.
erased :: Int -> Text.Text
erased w = Text.pack "\r" <> Text.replicate w (Text.pack " ") <> Text.pack "\r"

-- | The progress line: the cases run of @count@, the failures among them, and
-- the minutes and seconds since the first started.
progressLine :: Natural -> Summary -> Natural -> Text.Text
progressLine count tally elapsed =
  let (minutes, seconds) = divMod (div elapsed 1000000000) 60
   in Text.pack (show (total tally) <> "/" <> show count <> " run, " <> show (failures tally) <> " failed, " <> show minutes <> ":" <> (if seconds < 10 then "0" else "") <> show seconds)

-- | A command running only the cases that failed, one pattern each, when any
-- did.
rerunLine :: [Result] -> Maybe Text.Text
rerunLine ran = case List.sort [title r | r <- ran, Maybe.isJust (failure r)] of
  [] -> Nothing
  failing -> Just . Text.unwords $ Text.pack "Rerun the failures with: cabal run parun -- test" : foldMap (\t -> [Text.pack "-p", quoted t]) failing
  where
    quoted t = Text.pack "'" <> Text.replace (Text.pack "'") (Text.pack "'\\''") t <> Text.pack "'"

succeeded :: Summary -> Bool
succeeded summary = total summary > 0 && failures summary == 0

-- | The last line of a run. An empty selection is neither a pass nor a
-- failure, so it reads differently from both.
summaryLine :: Key.Type.Key -> Summary -> Text.Text
summaryLine s summary
  | total summary == 0 = Text.pack "Matched no tests with seed " <> Key.toHex s <> Text.pack "."
  | failures summary == 0 = Text.pack ("Passed all " <> show (total summary) <> " tests with seed ") <> Key.toHex s <> Text.pack "."
  | otherwise = Text.pack ("Failed " <> show (failures summary) <> " of " <> show (total summary) <> " tests with seed ") <> Key.toHex s <> Text.pack "."
