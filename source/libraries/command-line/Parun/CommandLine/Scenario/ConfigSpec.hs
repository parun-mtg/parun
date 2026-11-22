module Parun.CommandLine.Scenario.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.CommandLine.Scenario.Config as Config
import qualified Parun.CommandLine.Scenario.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Scenario.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads every file, in order" $
      Spec.assertEq s (configured [] ["a.json", "b.json"]) (Right Config.MkConfig {Config.perturb = False, Config.files = ["a.json", "b.json"], Config.help = False})
    Spec.it s "sets each switch" $
      Spec.assertEq s (fmap switches $ configured [Flag.Perturb True, Flag.Help True] ["a.json"]) (Right (True, True))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap switches $ configured [Flag.Perturb True, Flag.Help True, Flag.Perturb False, Flag.Help False] ["a.json"]) (Right (False, False))
    Spec.it s "needs no file when asked for the usage" $
      Spec.assertEq s (configured [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing file" $
      Spec.assertEq s (configured [Flag.Perturb True] []) (Left "missing argument: FILE")
  where
    configured :: [Flag.Flag] -> [String] -> Either String Config.Config
    configured fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags fs
    switches c = (Config.perturb c, Config.help c)
