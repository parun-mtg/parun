module Parun.CommandLine.Deck.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.CommandLine.Deck.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Deck.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given, in any order beside the file" $
      Spec.assertEq s (parsed ["--pool", "p.txt", "d.txt", "--format=sealed", "-h", "--no-help"]) (Right ([Flag.Pool "p.txt", Flag.Format "sealed", Flag.Help True, Flag.Help False], ["d.txt"]))
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--formats", "x"]) (Left "unknown option: --formats")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--pool"]) (Left "option `--pool' requires an argument FILE")
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
