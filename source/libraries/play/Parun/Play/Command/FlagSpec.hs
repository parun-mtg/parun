module Parun.Play.Command.FlagSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Parun.Play.Command.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Play.Command.Flag" $ do
  Spec.describe s "fromArguments" $ do
    Spec.it s "reads each flag, in order, its argument as given, in any order beside the decks" $
      Spec.assertEq
        s
        (parsed ["--seed", "x", "a.txt", "--first=2", "--bot", "1", "--json", "--no-json", "--save", "g.json", "b.txt", "--arrange-graveyard", "2", "--format", "pauper", "--level", "easy", "--persona", "aggro", "--meta", "m.txt", "--pool", "vintage", "--object-limit", "9", "--think-limit", "0", "-h", "--no-help"])
        ( Right
            ( [Flag.Seed "x", Flag.First "2", Flag.Bot "1", Flag.Json True, Flag.Json False, Flag.Save "g.json", Flag.ArrangeGraveyard "2", Flag.Format "pauper", Flag.Level "easy", Flag.Persona "aggro", Flag.Meta "m.txt", Flag.Pool "vintage", Flag.ObjectLimit "9", Flag.ThinkLimit "0", Flag.Help True, Flag.Help False],
              ["a.txt", "b.txt"]
            )
        )
    Spec.it s "reads nothing from nothing" $
      Spec.assertEq s (parsed []) (Right ([], []))
    Spec.it s "names an unknown option" $
      Spec.assertEq s (parsed ["--bots", "1"]) (Left "unknown option: --bots")
    Spec.it s "refuses an option missing its argument" $
      Spec.assertEq s (parsed ["--first"]) (Left "option `--first' requires an argument SEAT")
  where
    parsed :: [String] -> Either String ([Flag.Flag], [String])
    parsed = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Flag.fromArguments
