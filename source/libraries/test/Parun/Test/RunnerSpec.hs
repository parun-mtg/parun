module Parun.Test.RunnerSpec where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified GHC.Clock as Clock
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Test.Runner as Runner
import qualified Parun.Types.Key as Key.Type

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Test.Runner" $ do
  Spec.describe s "parseOptions" $ do
    Spec.it s "collects every pattern, in order" $
      Spec.assertEq s (fmap Runner.patterns $ Runner.parseOptions ["-p", "a", "--pattern=b c", "-pd"]) (Right [Text.pack "a", Text.pack "b c", Text.pack "d"])
    Spec.it s "reads the CI's timeout" $
      Spec.assertEq s (fmap Runner.timeout $ Runner.parseOptions ["--timeout=120s"]) (Right (Just 120000000))
    Spec.it s "reads a seed" $
      Spec.assertEq s (fmap Runner.seed $ Runner.parseOptions ["--seed", replicate 63 '0' <> "7"]) (Right (Just $ key 7))
    Spec.it s "selects every case when given nothing" $
      Spec.assertEq s (Runner.parseOptions []) (Right Runner.MkOptions {Runner.patterns = [], Runner.timeout = Nothing, Runner.seed = Nothing, Runner.json = Nothing})
    Spec.it s "refuses a positional argument" $
      Spec.assertEq s (Runner.parseOptions ["Engine"]) (Left "unexpected argument: Engine")
    Spec.it s "refuses a flag it does not know" $
      Spec.assertBool s (isLeft $ Runner.parseOptions ["--hide-successes"]) "accepted --hide-successes"
    Spec.it s "reads where to write the report" $
      Spec.assertEq s (fmap Runner.json $ Runner.parseOptions ["--json", "report.json"]) (Right (Just "report.json"))
    Spec.it s "refuses a seed that is not a key" $
      Spec.assertEq s (Runner.parseOptions ["--seed", "x"]) (Left "not a seed: x")

  Spec.describe s "duration" $ do
    Spec.it s "reads bare seconds" $
      Spec.assertEq s (Runner.duration "3") (Just 3000000)
    Spec.it s "reads each unit" $
      Spec.assertEq s (fmap Runner.duration ["500ms", "2s", "2m", "1h"]) [Just 500000, Just 2000000, Just 120000000, Just 3600000000]
    Spec.it s "refuses a unit alone, and an unknown unit" $
      Spec.assertEq s (fmap Runner.duration ["", "s", "2d"]) [Nothing, Nothing, Nothing]

  Spec.describe s "runner" $ do
    Spec.it s "joins a case's groups and name with dots" $ do
      cases <- collect $ Spec.describe r "a" (Spec.describe r "b" (Spec.it r "c" (pure ())) >> Spec.it r "d" (pure ()))
      Spec.assertEq s (fmap Runner.name $ Foldable.toList cases) [Text.pack "a.b.c", Text.pack "a.d"]

  Spec.describe s "selected" $ do
    let options ps = Runner.MkOptions {Runner.patterns = fmap Text.pack ps, Runner.timeout = Nothing, Runner.seed = Nothing, Runner.json = Nothing}
        named = Runner.MkCase {Runner.path = fmap Text.pack ["Engine", "Trigger", "fires once"], Runner.action = pure ()}
    Spec.it s "matches a substring spanning groups" $
      Spec.assertBool s (Runner.selected (options ["Trigger.fires"]) named) "missed Trigger.fires"
    Spec.it s "matches when any pattern does" $
      Spec.assertBool s (Runner.selected (options ["Combat", "once"]) named) "missed once"
    Spec.it s "misses when no pattern does" $
      Spec.assertBool s (not $ Runner.selected (options ["Combat", "twice"]) named) "matched neither"
    Spec.it s "is case-sensitive" $
      Spec.assertBool s (not $ Runner.selected (options ["engine"]) named) "matched engine"

  Spec.describe s "ordered" $ do
    let options = Runner.MkOptions {Runner.patterns = [], Runner.timeout = Nothing, Runner.seed = Nothing, Runner.json = Nothing}
        cases = Seq.fromList $ fmap (\i -> Runner.MkCase {Runner.path = [Text.pack $ show i], Runner.action = pure ()}) [1 .. 20 :: Integer]
        names seed = fmap Runner.name $ Runner.ordered options (key seed) cases
    Spec.it s "gives one seed one order" $
      Spec.assertEq s (names 1) (names 1)
    Spec.it s "gives two seeds different orders of the same cases" $ do
      Spec.assertNe s (names 1) (names 2)
      Spec.assertEq s (Seq.sort . Seq.fromList $ names 1) (Seq.sort . Seq.fromList $ names 2)
    Spec.it s "drops the cases no pattern selects" $
      Spec.assertEq s (Seq.sort . Seq.fromList . fmap Runner.name $ Runner.ordered options {Runner.patterns = [Text.pack "1"]} (key 1) cases) (Seq.fromList $ fmap Text.pack ["1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"])

  Spec.describe s "run" $ do
    Spec.it s "counts each kind of failure and reports it with its path" $ do
      cases <- collect . Spec.describe r "g" $ do
        Spec.it r "passes" $ pure ()
        Spec.it r "asserts" $ Spec.assertFailure r "boom"
        Spec.it r "throws" $ ioError (userError "bang")
        Spec.it r "hangs" $ Concurrent.threadDelay 10000000
      sink <- IORef.newIORef []
      ran <- Runner.run (failuresInto sink) (Just 10000) (Foldable.toList cases)
      Spec.assertEq s (Runner.summarize ran) Runner.MkSummary {Runner.total = 4, Runner.failures = 3}
      reported <- IORef.readIORef sink
      Spec.assertEq s (Seq.sort . Seq.fromList $ fmap (Text.takeWhile (/= ':')) reported) (Seq.fromList $ fmap Text.pack ["FAIL g.asserts", "FAIL g.hangs", "FAIL g.throws"])
      Spec.assertBool s (any (Text.isPrefixOf $ Text.pack "FAIL g.hangs: timed out") reported) "no timeout line"
      Spec.assertBool s (any (Text.isPrefixOf $ Text.pack "FAIL g.throws: user error (bang)") reported) "no exception line"
    Spec.it s "carries an assertion's call stack into its failure line" $ do
      cases <- collect . Spec.it r "asserts" $ Spec.assertFailure r "boom"
      sink <- IORef.newIORef []
      _ <- Runner.run (failuresInto sink) Nothing (Foldable.toList cases)
      reported <- IORef.readIORef sink
      case reported of
        [l] -> do
          Spec.assertBool s (Text.isPrefixOf (Text.pack "FAIL asserts: boom\n  CallStack") l) $ "unexpected line: " <> Text.unpack l
          Spec.assertBool s (Text.isInfixOf (Text.pack "Parun/Test/RunnerSpec.hs") $ Text.replace (Text.pack "\\") (Text.pack "/") l) $ "no call site: " <> Text.unpack l
        _ -> Spec.assertFailure s $ "expected one line, got " <> show reported
    Spec.it s "leaves out the frames inside Parun.Spec" $ do
      cases <- collect . Spec.it r "asserts" $ Spec.assertEq r 'a' 'b'
      sink <- IORef.newIORef []
      _ <- Runner.run (failuresInto sink) Nothing (Foldable.toList cases)
      reported <- fmap (Text.replace (Text.pack "\\") (Text.pack "/")) <$> IORef.readIORef sink
      Spec.assertBool s (any (Text.isInfixOf $ Text.pack "Parun/Test/RunnerSpec.hs") reported && not (any (Text.isInfixOf $ Text.pack "Parun/Spec.hs") reported)) $ "unexpected frames: " <> show reported
    Spec.it s "sites an assertion at the case's own line, and an exception nowhere" $ do
      cases <- collect $ do
        Spec.it r "asserts" $ Spec.assertEq r 'a' 'b'
        Spec.it r "throws" $ ioError (userError "bang")
      ran <- Runner.run (const $ pure ()) Nothing (Foldable.toList cases)
      let siteOf n = fmap Runner.site $ filter ((== Text.pack n) . Runner.title) ran
      case siteOf "asserts" of
        [Just w] -> Spec.assertBool s (Text.isSuffixOf (Text.pack "Parun/Test/RunnerSpec.hs") (Runner.sourceFile w) && Runner.sourceLine w > 0) $ "not this file: " <> show w
        other -> Spec.assertFailure s $ "expected one site, got " <> show other
      Spec.assertEq s (siteOf "throws") [Nothing]
    Spec.it s "answers each case with its path, its failure and how long it ran" $ do
      cases <- collect . Spec.describe r "g" $ do
        Spec.it r "quick" $ pure ()
        Spec.it r "slow" $ Concurrent.threadDelay 50000 >> Spec.assertFailure r "boom"
      ran <- Runner.run (const $ pure ()) Nothing (Foldable.toList cases)
      let named n = filter ((== Text.pack n) . Runner.title) ran
      Spec.assertEq s (fmap Runner.failure $ named "g.quick") [Nothing]
      Spec.assertBool s (all (maybe False (Text.isPrefixOf $ Text.pack "boom") . Runner.failure) (named "g.slow") && length (named "g.slow") == 1) $ "no failure: " <> show ran
      Spec.assertBool s (all ((>= 50000000) . Runner.nanoseconds) $ named "g.slow") $ "too quick: " <> show ran

  Spec.describe s "assertEqWith" $ do
    Spec.it s "names the actual value and the expected one on lines of their own" $ do
      cases <- collect . Spec.it r "asserts" $ Spec.assertEqWith r "label" 'a' 'b'
      ran <- Runner.run (const $ pure ()) Nothing (Foldable.toList cases)
      Spec.assertEq s (fmap (fmap (Text.takeWhile (/= 'C')) . Runner.failure) ran) [Just $ Text.pack "label: expected actual == expected\n  actual:   'a'\n  expected: 'b'\n"]

  Spec.describe s "shown" $ do
    let passed = Runner.MkResult (Text.pack "g.a") Nothing Nothing 5
        failed = Runner.MkResult (Text.pack "g.b") (Just $ Text.pack "boom") Nothing 5
        plain = Runner.MkDisplay {Runner.live = False, Runner.annotated = False}
        terminal = plain {Runner.live = True}
        written display results = do
          sink <- IORef.newIORef []
          started <- Clock.getMonotonicTimeNSec
          screen <- IORef.newIORef Runner.MkScreen {Runner.counted = Runner.MkSummary 0 0, Runner.drawnAt = 0, Runner.width = 0}
          Foldable.traverse_ (Runner.shown (\t -> IORef.modifyIORef' sink (t :)) display (Natural.length results) started screen) results
          reverse <$> IORef.readIORef sink
    Spec.it s "prints only failures when not at a terminal" $ do
      writes <- written plain [passed, failed, passed]
      Spec.assertEq s writes [Text.pack "FAIL g.b: boom\n"]
    Spec.it s "at a terminal, blanks the progress line for a failure and redraws it beneath" $ do
      writes <- written terminal [passed, failed]
      Spec.assertEq s writes $ fmap Text.pack ["\r1/2 run, 0 failed, 0:00", "\r" <> fmap (const ' ') "1/2 run, 0 failed, 0:00" <> "\r", "FAIL g.b: boom\n", "\r2/2 run, 1 failed, 0:00"]
    Spec.it s "at a terminal, draws the last case however soon it lands" $ do
      writes <- written terminal [passed, passed, passed]
      Spec.assertEq s (last writes) (Text.pack "\r3/3 run, 0 failed, 0:00")
    Spec.it s "under GitHub Actions, annotates a failure after its line" $ do
      writes <- written plain {Runner.annotated = True} [passed, failed]
      Spec.assertEq s writes $ fmap Text.pack ["FAIL g.b: boom\n", "::error title=g.b::boom\n"]

  Spec.describe s "annotation" $ do
    Spec.it s "names the file and line, escaping the title's and message's specials" $
      Spec.assertEq s (Runner.annotation (Text.pack "g.a: 1, 2") (Just Runner.MkSite {Runner.sourceFile = Text.pack "source/A.hs", Runner.sourceLine = 7}) (Text.pack "50%\r\nCallStack")) (Text.pack "::error file=source/A.hs,line=7,title=g.a%3A 1%2C 2::50%25%0D%0ACallStack")

  Spec.describe s "progressLine" $ do
    Spec.it s "counts the cases and failures and pads the seconds" $
      Spec.assertEq s (Runner.progressLine 30898 Runner.MkSummary {Runner.total = 12034, Runner.failures = 1} 125000000000) (Text.pack "12034/30898 run, 1 failed, 2:05")

  Spec.describe s "reportCodec" $ do
    Spec.it s "writes the seed and each case, a failure and its site only where there is one" $
      Spec.assertEq
        s
        (Common.render $ Codec.encode Runner.reportCodec Runner.MkReport {Runner.shuffled = Text.pack "07", Runner.results = [Runner.MkResult (Text.pack "g.a") Nothing Nothing 5, Runner.MkResult (Text.pack "g.b") (Just $ Text.pack "boom") (Just Runner.MkSite {Runner.sourceFile = Text.pack "source/A.hs", Runner.sourceLine = 7}) 900]})
        (Text.pack "{\"seed\":\"07\",\"cases\":[{\"name\":\"g.a\",\"nanoseconds\":5},{\"name\":\"g.b\",\"failure\":\"boom\",\"site\":{\"file\":\"source/A.hs\",\"line\":7},\"nanoseconds\":900}]}")

  Spec.describe s "summaryLine" $ do
    Spec.it s "names the seed in a pass" $
      Spec.assertEq s (Runner.summaryLine (key 9) Runner.MkSummary {Runner.total = 3, Runner.failures = 0}) (Text.pack ("Passed all 3 tests with seed " <> nine <> "."))
    Spec.it s "names the seed in a failure" $
      Spec.assertEq s (Runner.summaryLine (key 9) Runner.MkSummary {Runner.total = 3, Runner.failures = 2}) (Text.pack ("Failed 2 of 3 tests with seed " <> nine <> "."))
    Spec.it s "tells an empty selection apart from a pass" $ do
      Spec.assertEq s (Runner.summaryLine (key 9) Runner.MkSummary {Runner.total = 0, Runner.failures = 0}) $ Text.pack ("Matched no tests with seed " <> nine <> ".")
      Spec.assertBool s (not $ Runner.succeeded Runner.MkSummary {Runner.total = 0, Runner.failures = 0}) "an empty selection succeeded"
  where
    -- Keeps each failure's line, as the runner prints it.
    failuresInto :: IORef.IORef [Text.Text] -> Runner.Result -> IO ()
    failuresInto sink result = Foldable.for_ (Runner.failure result) $ \m -> IORef.atomicModifyIORef' sink $ \ls -> (Runner.failureLine (Runner.title result) m : ls, ())
    collect :: Writer.WriterT (Seq.Seq Runner.Case) IO () -> IO (Seq.Seq Runner.Case)
    collect = Writer.execWriterT
    r :: Spec.Spec IO (Writer.WriterT (Seq.Seq Runner.Case) IO)
    r = Runner.runner
    -- The key whose hexadecimal digits are 63 zeros and this one.
    key :: Integer -> Key.Type.Key
    key n = Maybe.fromJust . Key.fromHex . Text.pack $ replicate 63 '0' <> show n
    nine = replicate 63 '0' <> "9"
    isLeft e = case e of
      Left _ -> True
      Right _ -> False
