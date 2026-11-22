module Parun.Benchmark.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.Benchmark.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Benchmark.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given" $
      Spec.assertEq s (parsed ["--json", "bench.json", "-p", "legal", "--pattern=goldfish 2p", "-h", "--no-help"]) (Right [Flag.Json "bench.json", Flag.Pattern "legal", Flag.Pattern "goldfish 2p", Flag.Help True, Flag.Help False])
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right [])
    Spec.it s "refuses a positional argument" $
      Spec.assertEq s (parsed ["legal"]) (Left "unexpected argument: legal")
    Spec.it s "names a tasty-bench flag as unknown" $
      Spec.assertEq s (parsed ["--csv", "bench.csv"]) (Left "unknown option: --csv")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--json"]) (Left "option `--json' requires an argument FILE")
  where
    parsed :: [String] -> Either String [Flag.Flag]
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
