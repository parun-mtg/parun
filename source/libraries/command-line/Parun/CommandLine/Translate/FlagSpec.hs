module Parun.CommandLine.Translate.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.CommandLine.Translate.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Translate.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given, anywhere among the positionals" $
      Spec.assertEq s (parsed ["m.json", "c.json", "--set", "LEA", "Shock", "--out=dir", "-h", "--no-help", "@names.txt"]) (Right ([Flag.Set "LEA", Flag.Out "dir", Flag.Help True, Flag.Help False], ["m.json", "c.json", "Shock", "@names.txt"]))
    Spec.it s "reads a name beginning with - after a --" $
      Spec.assertEq s (parsed ["m.json", "c.json", "--", "-Odd Name"]) (Right ([], ["m.json", "c.json", "-Odd Name"]))
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names a mistyped flag rather than reading it as names" $
      Spec.assertEq s (parsed ["m.json", "c.json", "--sets", "LEA"]) (Left "unknown option: --sets")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["m.json", "c.json", "--out"]) (Left "option `--out' requires an argument DIR")
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
