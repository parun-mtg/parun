module Parun.Test.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.Spec as Spec
import qualified Parun.Test.Flag as Flag

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Test.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given" $
      Spec.assertEq s (parsed ["-p", "a", "--pattern=b c", "-pd", "--seed", "x", "--timeout=2d", "--no-list"]) (Right [Flag.Pattern "a", Flag.Pattern "b c", Flag.Pattern "d", Flag.Seed "x", Flag.Timeout "2d", Flag.List False])
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right [])
    Spec.it s "refuses a positional argument" $
      Spec.assertEq s (parsed ["Engine"]) (Left "unexpected argument: Engine")
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--hide-successes"]) (Left "unknown option: --hide-successes")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--seed"]) (Left "option `--seed' requires an argument KEY")
  where
    parsed :: [String] -> Either String [Flag.Flag]
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
