module Parun.CommandLine.CompletionSpec where

import qualified Parun.CommandLine.Completion as Completion
import qualified Parun.CommandLine.Completion.Command as Command
import qualified Parun.CommandLine.Completion.Reply as Reply
import qualified Parun.CommandLine.Completion.Value as Value
import qualified Parun.CommandLine.Deck.Config as Deck.Config
import qualified Parun.CommandLine.Scenario.Config as Scenario.Config
import qualified Parun.CommandLine.Translate.Config as Translate.Config
import qualified Parun.Spec as Spec
import qualified System.Console.GetOpt as GetOpt

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Completion" $ do
  Spec.describe s "complete" $ do
    Spec.it s "offers the commands beginning with the first word" $
      Spec.assertEq s (fmap (completed 0) [["t"], ["p"], []]) [candidates ["translate"], candidates ["probe"], candidates ["deck", "translate", "scenario", "probe"]]
    Spec.it s "offers a command's options beginning with a dash" $
      Spec.assertEq s (completed 1 ["deck", "--f"]) (candidates ["--format"])
    Spec.it s "offers every short and long option" $
      Spec.assertEq s (completed 1 ["deck", "-"]) (candidates ["-h", "--help", "--no-help", "--format", "--pool"])
    Spec.it s "offers an option's choices in the next word" $
      Spec.assertEq s (completed 2 ["deck", "--format", "m"]) (candidates ["modern"])
    Spec.it s "offers an option's choices after its =" $
      Spec.assertEq s (completed 1 ["deck", "--format=v"]) (candidates ["--format=vintage"])
    Spec.it s "reads a long option's prefix as GetOpt does" $
      Spec.assertEq s (completed 2 ["deck", "--form", ""]) (candidates ["modern", "vintage"])
    Spec.it s "offers a per-seat value's choices, and after a seat and =" $
      Spec.assertEq s (fmap (\word -> completed 2 ["probe", "--level", word]) ["", "2=h", "3=h"]) [candidates ["easy", "hard"], candidates ["2=hard"], candidates []]
    Spec.it s "asks for a file name, after the part of the word that is not one" $
      Spec.assertEq s (fmap (completed 1) [["deck", "x"], ["deck", "--pool=x"], ["probe", "--weights=1=x"]]) [Reply.MkFiles "", Reply.MkFiles "--pool=", Reply.MkFiles "--weights=1="]
    Spec.it s "asks for a file name behind a prefix, and nothing without it" $
      Spec.assertEq s (fmap (\word -> completed 3 ["translate", "a", "b", word]) ["@x", "x"]) [Reply.MkFiles "@", candidates []]
    Spec.it s "counts positional arguments around options and their arguments" $
      Spec.assertEq s (fmap (completed 5) [["translate", "--out", "dir", "a", "-h", "@x"], ["translate", "a", "--set", "lea", "b", "@x"]]) [Reply.MkFiles "", Reply.MkFiles "@"]
    Spec.it s "reads only positional arguments after --" $
      Spec.assertEq s (fmap (completed 2) [["scenario", "--", "-"], ["deck", "--", "-"]]) [Reply.MkFiles "", Reply.MkFiles ""]
    Spec.it s "offers the options where no positional argument is taken" $
      Spec.assertEq s (completed 2 ["deck", "a", ""]) (candidates ["-h", "--help", "--no-help", "--format", "--pool"])
    Spec.it s "offers nothing after -- where no positional argument is taken" $
      Spec.assertEq s (completed 3 ["deck", "--", "a", ""]) (candidates [])
    Spec.it s "reads a short option awaiting its argument only at a cluster's end" $
      Spec.assertEq s (fmap (completed 2) [["probe", "-vl", ""], ["probe", "-lv", ""]]) [candidates ["easy", "hard"], Reply.MkFiles ""]
    Spec.it s "offers nothing for an option's value no list holds" $
      Spec.assertEq s (completed 2 ["translate", "--set", ""]) (candidates [])
    Spec.it s "offers nothing for an unknown command" $
      Spec.assertEq s (fmap (completed 1) [["nope", ""], ["nope", "-"]]) [candidates [], candidates []]
    Spec.it s "takes an absent word at the cursor as empty" $
      Spec.assertEq s (completed 2 ["deck", "--format"]) (candidates ["modern", "vintage"])
  Spec.describe s "Reply.render" $ do
    Spec.it s "writes a candidate a line" $
      Spec.assertEq s (Reply.render (candidates ["flat", "tree"])) "flat\ntree\n"
    Spec.it s "writes a file name's prefix after :files" $
      Spec.assertEq s (fmap Reply.render [Reply.MkFiles "", Reply.MkFiles "@"]) [":files\n", ":files@\n"]
  where
    candidates = Reply.MkCandidates
    completed = Completion.complete ["modern", "vintage"] commands
    commands = [Deck.Config.command, Translate.Config.command, Scenario.Config.command, probe]
    -- A command with a per-seat option and a short option cluster.
    probe =
      (Command.new "probe" [GetOpt.Option "v" [] (GetOpt.NoArg ()) "", GetOpt.Option "l" ["level"] (GetOpt.ReqArg (const ()) "LEVEL") "", GetOpt.Option [] ["weights"] (GetOpt.ReqArg (const ()) "FILE") ""])
        { Command.values = [("level", Value.MkSeated ["1", "2"] (Value.MkChoices ["easy", "hard"])), ("weights", Value.MkSeated ["1", "2"] Value.MkFiles)],
          Command.rest = Just Value.MkFiles
        }
