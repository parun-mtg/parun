module Parun.Play.Match.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.CommandLine.Completion as Completion
import qualified Parun.CommandLine.Completion.Reply as Reply
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Match.Config as Config
import qualified Parun.Play.Match.Flag as Flag
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Spec as Spec
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.Playout as Playout
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.SearchMethod as SearchMethod

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Play.Match.Config" $ do
  Spec.describe s "command" $ do
    Spec.it s "completes a value to every name its flag reads, in every seat and in one" $
      Spec.assertEq
        s
        (fmap (\(flag, word) -> completed ["match", flag, word]) [("--method", ""), ("--playout", "2="), ("--level", "1=m"), ("--persona", ""), ("--search", "")])
        [ Reply.MkCandidates ["flat", "tree"],
          Reply.MkCandidates ["2=random", "2=greedy", "2=polar", "2=classified"],
          Reply.MkCandidates ["1=medium"],
          Reply.MkCandidates ["balanced", "aggro", "control"],
          Reply.MkCandidates ["1", "2"]
        ]
    Spec.it s "reads every name it completes" $
      Spec.assertEq s (fmap Config.methodNamed ["flat", "tree"], fmap Config.playoutNamed ["random", "greedy", "polar", "classified"]) (fmap Just [minBound .. maxBound], fmap Just [minBound .. maxBound])
    Spec.it s "completes a deck, a format and a weights file" $
      Spec.assertEq s (fmap completed [["match", ""], ["match", "--pool", "p"], ["match", "--weights", "1=w"]]) [Reply.MkFiles "", Reply.MkCandidates ["pauper"], Reply.MkFiles "1="]
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads the decks alone" $
      Spec.assertEq s (configured [] ["a.txt", "b.txt"]) (Right Config.MkConfig {Config.options = MatchOptions.none, Config.files = ["a.txt", "b.txt"], Config.help = False})
    Spec.it s "reads the match's own flags" $
      Spec.assertEq
        s
        (options [Flag.Seed key, Flag.Games "4", Flag.Save "out", Flag.Positions "p.tsv", Flag.Search "2", Flag.Meta "m.txt", Flag.Pool "pauper"])
        (Right MatchOptions.none {MatchOptions.seed = Key.fromHex (Text.pack key), MatchOptions.games = 4, MatchOptions.save = Just "out", MatchOptions.positions = Just "p.tsv", MatchOptions.searching = Set.singleton 2, MatchOptions.meta = Just "m.txt", MatchOptions.pool = Just (Text.pack "pauper")})
    Spec.it s "sets each budget flag in every seat" $
      Spec.assertEq
        s
        (fmap (Map.lookup 1 . MatchOptions.budgets) $ options [Flag.Samples "3", Flag.Turns "1", Flag.Cutoff "9", Flag.Playout "polar", Flag.Method "tree", Flag.Candidates "5", Flag.Mistakes "10"])
        (Right (Just MatchOptions.budget {SearchBudget.samples = 3, SearchBudget.turns = 1, SearchBudget.prompts = 9, SearchBudget.playout = Playout.Polar, SearchBudget.method = SearchMethod.InformationSet, SearchBudget.candidates = 5, SearchBudget.mistakes = 10}))
    Spec.it s "sets a budget flag with a seat in that seat alone" $
      Spec.assertEq
        s
        (fmap MatchOptions.budgets $ options [Flag.Samples "2=3", Flag.Level "1=easy"])
        (Right (Map.fromList [(1, Opponent.budgetAt Difficulty.MkEasy MatchOptions.budget), (2, MatchOptions.budget {SearchBudget.samples = 3})]))
    Spec.it s "weighs every seat, then one, the last flag for a seat winning" $
      Spec.assertEq s (fmap MatchOptions.weighing $ options [Flag.Persona "aggro", Flag.Weights "2=w.json"]) (Right (Map.fromList [(1, Left Persona.MkAggro), (2, Right "w.json")]))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap Config.help $ configured [Flag.Help True, Flag.Help False] ["a.txt", "b.txt"]) (Right False)
    Spec.it s "needs no deck when asked for the usage" $
      Spec.assertEq s (configured [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing deck" $
      Spec.assertEq s (configured [] ["a.txt"]) (Left "missing argument: DECK")
    Spec.it s "refuses a third deck" $
      Spec.assertEq s (configured [] ["a.txt", "b.txt", "c.txt"]) (Left "unexpected argument: c.txt")
    Spec.it s "refuses each flag's bad argument in its own words" $
      Spec.assertEq
        s
        (fmap (\f -> Either.fromLeft "accepted" (configured [f] ["a.txt", "b.txt"])) [Flag.Seed "x", Flag.Games "many", Flag.Search "3", Flag.Samples "0", Flag.Turns "3=1", Flag.Cutoff "-", Flag.Playout "smart", Flag.Method "deep", Flag.Candidates "x", Flag.Mistakes "101", Flag.Level "impossible", Flag.Persona "timid"])
        [ "not a seed: x",
          "not a number of games: many",
          "not a seat: 3",
          "not a number of samples: 0",
          "not a number of turns: 3=1",
          "not a number of prompts: -",
          "not a playout policy, random, greedy, polar or classified: smart",
          "not a search method, flat or tree: deep",
          "not a number of candidate worlds: x",
          "not a percent of decisions: 101",
          "not a level, easy, medium or hard: impossible",
          "not a persona: timid"
        ]
  where
    configured :: [Flag.Flag] -> [String] -> Either String Config.Config
    configured fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags ["DECK", "DECK"] (Just 2) fs
    options fs = fmap Config.options $ configured fs ["a.txt", "b.txt"]
    key = replicate 63 '0' <> "7"
    completed typed = Completion.complete ["pauper", "vintage"] [Config.command] (List.genericLength typed - 1) typed
