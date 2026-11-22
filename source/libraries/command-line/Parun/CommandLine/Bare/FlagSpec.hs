module Parun.CommandLine.Bare.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.CommandLine.Bare.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Bare.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, beside the positionals" $
      Spec.assertEq s (parsed ["-h", "scenario", "--no-help", "--help"]) (Right ([Flag.Help True, Flag.Help False, Flag.Help True], ["scenario"]))
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--verbose"]) (Left "unknown option: --verbose")
  Spec.describe s "usage"
    . Spec.it s "heads the options with the command's synopsis"
    $ Spec.assertEq s (fmap (take 1 . lines) [Flag.usage "ingest" "FILE", Flag.usage "oracle" ""]) [["usage: parun ingest [OPTION...] FILE"], ["usage: parun oracle [OPTION...]"]]
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
