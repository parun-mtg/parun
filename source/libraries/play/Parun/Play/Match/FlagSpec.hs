module Parun.Play.Match.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.Play.Match.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Play.Match.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given, in any order beside the decks" $
      Spec.assertEq
        s
        (parsed ["--seed", "x", "a.txt", "--games=4", "--save", "out", "--positions", "p.tsv", "--search", "1", "--samples", "2=3", "--turns", "1", "--cutoff", "9", "b.txt", "--playout", "polar", "--method", "tree", "--candidates", "5", "--mistakes", "10", "--level", "2=easy", "--weights", "w.json", "--persona", "aggro", "--meta", "m.txt", "--pool", "pauper", "-h", "--no-help"])
        ( Right
            ( [Flag.Seed "x", Flag.Games "4", Flag.Save "out", Flag.Positions "p.tsv", Flag.Search "1", Flag.Samples "2=3", Flag.Turns "1", Flag.Cutoff "9", Flag.Playout "polar", Flag.Method "tree", Flag.Candidates "5", Flag.Mistakes "10", Flag.Level "2=easy", Flag.Weights "w.json", Flag.Persona "aggro", Flag.Meta "m.txt", Flag.Pool "pauper", Flag.Help True, Flag.Help False],
              ["a.txt", "b.txt"]
            )
        )
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--samps", "2"]) (Left "unknown option: --samps")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--games"]) (Left "option `--games' requires an argument N")
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
