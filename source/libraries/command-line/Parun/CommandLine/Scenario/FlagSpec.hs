module Parun.CommandLine.Scenario.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.CommandLine.Scenario.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Scenario.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, anywhere among the files" $
      Spec.assertEq s (parsed ["a.json", "--perturb", "b.json", "--no-perturb", "--help", "--no-help"]) (Right ([Flag.Perturb True, Flag.Perturb False, Flag.Help True, Flag.Help False], ["a.json", "b.json"]))
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--perturbed"]) (Left "unknown option: --perturbed")
    Spec.it s "refuses an argument given a switch" $
      Spec.assertEq s (parsed ["--perturb=yes"]) (Left "option `--perturb' doesn't allow an argument")
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
