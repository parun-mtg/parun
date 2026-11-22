{-# LANGUAGE ApplicativeDo #-}

-- | A benchmark harness on boot libraries alone. Each benchmark runs @n@ and
-- then @2n@ times, doubling @n@ until the two timings agree, and reports the
-- per-iteration mean and spread of a least-squares fit through both.
module Parun.Benchmark.Harness where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Clock as Clock
import qualified GHC.Stats as Stats
import qualified Numeric.Natural as Natural
import qualified Parun.Benchmark.Config as Config
import qualified Parun.Benchmark.Flag as Flag
import qualified Parun.CommandLine as CommandLine
import qualified Parun.Extra.Word64 as Word64
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified System.Mem as Mem

-- | A named action that runs its workload the given number of times.
data Benchmark = MkBenchmark
  { name :: Text.Text,
    run :: Natural.Natural -> IO ()
  }

-- | Times @f x@ to weak head normal form. @f@ and @x@ are passed through
-- every iteration, so GHC cannot float @f x@ out of the loop and time a cached
-- value.
whnf :: String -> (a -> b) -> a -> Benchmark
whnf n f x =
  MkBenchmark
    { name = Text.pack n,
      run = repeatWhnf f x
    }

repeatWhnf :: (a -> b) -> a -> Natural.Natural -> IO ()
repeatWhnf f x n = case n of
  0 -> pure ()
  _ -> do
    Monad.void . Exception.evaluate $ f x
    repeatWhnf f x (n - 1)
{-# NOINLINE repeatWhnf #-}

-- | What @n@ iterations cost: wall-clock nanoseconds, and bytes allocated when
-- the RTS keeps statistics (@+RTS -T@).
data Measurement = MkMeasurement
  { nanoseconds :: Natural.Natural,
    bytes :: Maybe Natural.Natural
  }
  deriving (Eq, Show)

-- | One benchmark's per-iteration figures.
data Result = MkResult
  { label :: Text.Text,
    meanPicoseconds :: Natural.Natural,
    stdevPicoseconds :: Natural.Natural,
    allocatedBytes :: Maybe Natural.Natural
  }
  deriving (Eq, Show)

resultCodec :: Codec.Codec Result
resultCodec = Fields.object $ do
  l <- Fields.required "name" Common.text label
  m <- Fields.required "meanPicoseconds" Common.natural meanPicoseconds
  s <- Fields.required "stdevPicoseconds" Common.natural stdevPicoseconds
  a <- Fields.defaulted "allocatedBytes" Nothing (Common.maybe Common.natural) allocatedBytes
  pure MkResult {label = l, meanPicoseconds = m, stdevPicoseconds = s, allocatedBytes = a}

-- | Whether the config selects the benchmark: its name holds one of the
-- patterns, or there are none.
selected :: Config.Config -> Benchmark -> Bool
selected config benchmark = null (Config.patterns config) || any (`Text.isInfixOf` name benchmark) (Config.patterns config)

-- | What the command line asks for, or the exit refusing it
-- ('CommandLine.configured'), read before any benchmark's input is.
configured :: IO Config.Config
configured = do
  arguments <- Environment.getArgs
  CommandLine.configured Flag.usage Config.help (Flag.fromArguments arguments >>= Config.fromFlags)

-- | Runs every benchmark the config selects, printing each result as it
-- lands.
defaultMain :: Config.Config -> [Benchmark] -> IO ()
defaultMain config benchmarks = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  results <- Monad.forM (filter (selected config) benchmarks) $ \benchmark -> do
    result <- measureBenchmark benchmark
    putStrLn $ renderResult result
    pure result
  Foldable.for_ (Config.json config) $ \file ->
    ByteString.writeFile file . Encoding.encodeUtf8 . Common.render . Codec.encode (Common.list resultCodec) $ results

measure :: Benchmark -> Natural.Natural -> IO Measurement
measure benchmark n = do
  enabled <- Stats.getRTSStatsEnabled
  Mem.performMinorGC
  allocatedBefore <- allocated enabled
  start <- Clock.getMonotonicTimeNSec
  run benchmark n
  end <- Clock.getMonotonicTimeNSec
  allocatedAfter <- allocated enabled
  pure
    MkMeasurement
      { nanoseconds = Word64.toNatural end - Word64.toNatural start,
        bytes = do
          before <- allocatedBefore
          after <- allocatedAfter
          pure $ after - before
      }

allocated :: Bool -> IO (Maybe Natural.Natural)
allocated enabled =
  if enabled
    then Just . Word64.toNatural . Stats.allocated_bytes <$> Stats.getRTSStats
    else pure Nothing

-- | Doubles the iterations until 'converged', or until 'budgetNanoseconds' is
-- spent, whichever comes first.
measureBenchmark :: Benchmark -> IO Result
measureBenchmark benchmark = do
  start <- Clock.getMonotonicTimeNSec
  let go n first = do
        second <- measure benchmark (2 * n)
        now <- Clock.getMonotonicTimeNSec
        let result = estimate (name benchmark) n first second
        if converged first second || Word64.toNatural now - Word64.toNatural start >= budgetNanoseconds
          then pure result
          else go (2 * n) second
  first <- measure benchmark 1
  go 1 first

-- | Past this, a benchmark reports what it has rather than doubling again.
budgetNanoseconds :: Natural.Natural
budgetNanoseconds = 100 * 1000000000

-- | The longer run is long enough to swamp the clock's resolution, and the
-- fit's spread is at most 5% of its mean.
converged :: Measurement -> Measurement -> Bool
converged first second =
  let (fit, squares) = fitTimes (nanoseconds first) (nanoseconds second)
   in nanoseconds second >= 10000000 && 400 * squares <= fit * fit

-- | The least-squares fit through the origin of @t1@ for @n@ iterations and
-- @t2@ for @2n@: the estimated time of @n@ iterations, and the sum of the
-- squared residuals.
fitTimes :: Natural.Natural -> Natural.Natural -> (Natural.Natural, Natural.Natural)
fitTimes t1 t2 =
  let fit = div (t1 + 2 * t2) 5
      residual a b = if a >= b then a - b else b - a
   in (fit, residual t1 fit ^ (2 :: Int) + residual t2 (2 * fit) ^ (2 :: Int))

estimate :: Text.Text -> Natural.Natural -> Measurement -> Measurement -> Result
estimate l n first second =
  let (fit, squares) = fitTimes (nanoseconds first) (nanoseconds second)
      perIteration x = div (1000 * x) n
   in MkResult
        { label = l,
          meanPicoseconds = perIteration fit,
          stdevPicoseconds = perIteration (squareRoot squares),
          allocatedBytes = do
            a1 <- bytes first
            a2 <- bytes second
            pure $ div (div (a1 + 2 * a2) 5) n
        }

-- | The floor of the square root, by Newton's method.
squareRoot :: Natural.Natural -> Natural.Natural
squareRoot x =
  let go guess =
        let next = div (guess + div x guess) 2
         in if next >= guess then guess else go next
   in case x of
        0 -> 0
        _ -> go x

renderResult :: Result -> String
renderResult result =
  Text.unpack (label result)
    <> ": "
    <> renderScaled picosecondUnits (meanPicoseconds result)
    <> " +- "
    <> renderScaled picosecondUnits (stdevPicoseconds result)
    <> case allocatedBytes result of
      Nothing -> ""
      Just a -> ", " <> renderScaled byteUnits a <> " allocated"

picosecondUnits :: [(Natural.Natural, String)]
picosecondUnits = [(1000000000000, "s"), (1000000000, "ms"), (1000000, "us"), (1000, "ns"), (1, "ps")]

byteUnits :: [(Natural.Natural, String)]
byteUnits = [(1000000000, "GB"), (1000000, "MB"), (1000, "KB"), (1, "B")]

-- | In the largest unit the value reaches, to three decimal places.
renderScaled :: [(Natural.Natural, String)] -> Natural.Natural -> String
renderScaled units x = case filter (\(size, _) -> x >= size) units of
  (size, unit) : _ ->
    let whole = div x size
        fraction = div (mod x size * 1000) size
        digits = show fraction
     in show whole <> "." <> replicate (3 - length digits) '0' <> digits <> " " <> unit
  [] -> "0 " <> foldMap snd (take 1 (reverse units))
