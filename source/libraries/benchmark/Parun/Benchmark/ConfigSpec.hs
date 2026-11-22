module Parun.Benchmark.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Parun.Benchmark.Config as Config
import qualified Parun.Benchmark.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Benchmark.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "selects every benchmark and writes nothing when given nothing" $
      Spec.assertEq s (configured []) (Right Config.MkConfig {Config.patterns = [], Config.json = Nothing, Config.help = False})
    Spec.it s "collects every pattern, in order" $
      Spec.assertEq s (fmap Config.patterns $ configured [Flag.Pattern "legal", Flag.Pattern "goldfish 2p"]) (Right [Text.pack "legal", Text.pack "goldfish 2p"])
    Spec.it s "reads where to write the results" $
      Spec.assertEq s (fmap Config.json $ configured [Flag.Json "bench.json"]) (Right (Just "bench.json"))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap Config.help $ configured [Flag.Help True, Flag.Help False]) (Right False)
  where
    configured :: [Flag.Flag] -> Either String Config.Config
    configured = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags
