module Parun.Test.RunnerSpec where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Spec as Spec
import qualified Parun.Test.Runner as Runner

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Test.Runner" $ do
  Spec.describe s "parseOptions" $ do
    Spec.it s "collects every pattern, in order" $
      Spec.assertEq s (fmap Runner.patterns $ Runner.parseOptions ["-p", "a", "--pattern=b c", "-pd"]) (Right [Text.pack "a", Text.pack "b c", Text.pack "d"])
    Spec.it s "reads the CI's timeout" $
      Spec.assertEq s (fmap Runner.timeout $ Runner.parseOptions ["--timeout=120s"]) (Right (Just 120000000))
    Spec.it s "reads a seed" $
      Spec.assertEq s (fmap Runner.seed $ Runner.parseOptions ["--seed", "7"]) (Right (Just 7))
    Spec.it s "selects every case when given nothing" $
      Spec.assertEq s (Runner.parseOptions []) (Right Runner.MkOptions {Runner.patterns = [], Runner.timeout = Nothing, Runner.seed = Nothing})
    Spec.it s "refuses a positional argument" $
      Spec.assertEq s (Runner.parseOptions ["Engine"]) (Left "unexpected argument: Engine")
    Spec.it s "refuses a flag it does not know" $
      Spec.assertBool s (isLeft $ Runner.parseOptions ["--hide-successes"]) "accepted --hide-successes"
    Spec.it s "refuses a seed that is not a number" $
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
    let options ps = Runner.MkOptions {Runner.patterns = fmap Text.pack ps, Runner.timeout = Nothing, Runner.seed = Nothing}
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
    let options = Runner.MkOptions {Runner.patterns = [], Runner.timeout = Nothing, Runner.seed = Nothing}
        cases = Seq.fromList $ fmap (\i -> Runner.MkCase {Runner.path = [Text.pack $ show i], Runner.action = pure ()}) [1 .. 20 :: Integer]
        names seed = fmap Runner.name $ Runner.ordered options seed cases
    Spec.it s "gives one seed one order" $
      Spec.assertEq s (names 1) (names 1)
    Spec.it s "gives two seeds different orders of the same cases" $ do
      Spec.assertNe s (names 1) (names 2)
      Spec.assertEq s (Seq.sort . Seq.fromList $ names 1) (Seq.sort . Seq.fromList $ names 2)
    Spec.it s "drops the cases no pattern selects" $
      Spec.assertEq s (Seq.sort . Seq.fromList . fmap Runner.name $ Runner.ordered options {Runner.patterns = [Text.pack "1"]} 1 cases) (Seq.fromList $ fmap Text.pack ["1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"])

  Spec.describe s "run" $ do
    Spec.it s "counts each kind of failure and reports it with its path" $ do
      cases <- collect . Spec.describe r "g" $ do
        Spec.it r "passes" $ pure ()
        Spec.it r "asserts" $ Spec.assertFailure r "boom"
        Spec.it r "throws" $ ioError (userError "bang")
        Spec.it r "hangs" $ Concurrent.threadDelay 10000000
      sink <- IORef.newIORef []
      summary <- Runner.run (\l -> IORef.atomicModifyIORef' sink $ \ls -> (l : ls, ())) Runner.MkOptions {Runner.patterns = [], Runner.timeout = Just 10000, Runner.seed = Nothing} 1 cases
      Spec.assertEq s summary Runner.MkSummary {Runner.total = 4, Runner.failures = 3}
      reported <- IORef.readIORef sink
      Spec.assertEq s (Seq.sort . Seq.fromList $ fmap (Text.takeWhile (/= ':')) reported) (Seq.fromList $ fmap Text.pack ["FAIL g.asserts", "FAIL g.hangs", "FAIL g.throws"])
      Spec.assertBool s (any (Text.isPrefixOf $ Text.pack "FAIL g.hangs: timed out") reported) "no timeout line"
      Spec.assertBool s (any (Text.isPrefixOf $ Text.pack "FAIL g.throws: user error (bang)") reported) "no exception line"
    Spec.it s "carries an assertion's call stack into its failure line" $ do
      cases <- collect . Spec.it r "asserts" $ Spec.assertFailure r "boom"
      sink <- IORef.newIORef []
      _ <- Runner.run (\l -> IORef.atomicModifyIORef' sink $ \ls -> (l : ls, ())) Runner.MkOptions {Runner.patterns = [], Runner.timeout = Nothing, Runner.seed = Nothing} 1 cases
      reported <- IORef.readIORef sink
      case reported of
        [l] -> do
          Spec.assertBool s (Text.isPrefixOf (Text.pack "FAIL asserts: boom\n  CallStack") l) $ "unexpected line: " <> Text.unpack l
          Spec.assertBool s (Text.isInfixOf (Text.pack "Parun/Test/RunnerSpec.hs") l) $ "no call site: " <> Text.unpack l
        _ -> Spec.assertFailure s $ "expected one line, got " <> show reported

  Spec.describe s "summaryLine" $ do
    Spec.it s "names the seed in a pass" $
      Spec.assertEq s (Runner.summaryLine 9 Runner.MkSummary {Runner.total = 3, Runner.failures = 0}) (Text.pack "Passed all 3 tests with seed 9.")
    Spec.it s "names the seed in a failure" $
      Spec.assertEq s (Runner.summaryLine 9 Runner.MkSummary {Runner.total = 3, Runner.failures = 2}) (Text.pack "Failed 2 of 3 tests with seed 9.")
    Spec.it s "tells an empty selection apart from a pass" $ do
      Spec.assertEq s (Runner.summaryLine 9 Runner.MkSummary {Runner.total = 0, Runner.failures = 0}) $ Text.pack "Matched no tests with seed 9."
      Spec.assertBool s (not $ Runner.succeeded Runner.MkSummary {Runner.total = 0, Runner.failures = 0}) "an empty selection succeeded"
  where
    collect :: Writer.WriterT (Seq.Seq Runner.Case) IO () -> IO (Seq.Seq Runner.Case)
    collect = Writer.execWriterT
    r :: Spec.Spec IO (Writer.WriterT (Seq.Seq Runner.Case) IO)
    r = Runner.runner
    isLeft e = case e of
      Left _ -> True
      Right _ -> False
