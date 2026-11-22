module Parun.Play.Command.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Either
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Play.Command.Config as Config
import qualified Parun.Play.Command.Flag as Flag
import qualified Parun.Play.Key as Key
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Spec as Spec
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Play.Command.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads the decks, each seat the bot's" $
      Spec.assertEq s (configured [Flag.Bot "1", Flag.Bot "2"] ["a.txt", "b.txt"]) (Right Config.MkConfig {Config.options = PlayOptions.none {PlayOptions.bots = Set.fromList [1, 2]}, Config.decks = ["a.txt", "b.txt"], Config.help = False})
    Spec.it s "reads every flag" $
      Spec.assertEq
        s
        (fmap Config.options $ configured [Flag.Seed key, Flag.First "3", Flag.Bot "1", Flag.Json True, Flag.Save "g.json", Flag.ArrangeGraveyard "2", Flag.Format "pauper", Flag.Level "easy", Flag.Persona "aggro", Flag.Meta "m.txt", Flag.Pool "vintage", Flag.ObjectLimit "9"] ["a.txt", "b.txt", "c.txt"])
        ( Right
            PlayOptions.none
              { PlayOptions.seed = Key.fromHex (Text.pack key),
                PlayOptions.first = Just 3,
                PlayOptions.bots = Set.singleton 1,
                PlayOptions.json = True,
                PlayOptions.save = Just "g.json",
                PlayOptions.arrangesGraveyard = Set.singleton 2,
                PlayOptions.format = Just (Text.pack "pauper"),
                PlayOptions.budget = Opponent.budgetAt Difficulty.MkEasy (PlayOptions.budget PlayOptions.none),
                PlayOptions.weights = Opponent.weightsOf Persona.MkAggro,
                PlayOptions.meta = Just "m.txt",
                PlayOptions.pool = Just (Text.pack "vintage"),
                PlayOptions.limits = Limits.standard {Limits.objects = 9}
              }
        )
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap (\c -> (PlayOptions.json (Config.options c), Config.help c)) $ configured [Flag.Bot "1", Flag.Bot "2", Flag.Json True, Flag.Help True, Flag.Json False, Flag.Help False] ["a.txt", "b.txt"]) (Right (False, False))
    Spec.it s "needs no deck when asked for the usage" $
      Spec.assertEq s (configured [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing deck" $
      Spec.assertEq s (configured [Flag.Json True] ["a.txt"]) (Left "missing argument: DECK")
    Spec.it s "refuses a seat past the last deck's" $
      Spec.assertEq s (configured [Flag.Json True, Flag.First "3"] ["a.txt", "b.txt"]) (Left "not a seat: 3")
    Spec.it s "refuses a seat nobody plays" $
      Spec.assertEq s (configured [Flag.Bot "1"] ["a.txt", "b.txt"]) (Left "seat 2 has no player: name it with --bot, or speak the line protocol with --json")
    Spec.it s "refuses each flag's bad argument in its own words" $
      Spec.assertEq
        s
        (fmap (\f -> Either.fromLeft "accepted" (configured [Flag.Json True, f] ["a.txt", "b.txt"])) [Flag.Seed "x", Flag.First "0", Flag.Bot "one", Flag.ArrangeGraveyard "", Flag.Level "impossible", Flag.Persona "timid", Flag.ObjectLimit "many"])
        [ "not a seed: x",
          "not a seat: 0",
          "not a seat: one",
          "not a seat: ",
          "not a level, easy, medium or hard: impossible",
          "not a persona, balanced, aggro or control: timid",
          "not a number of objects: many"
        ]
  where
    configured :: [Flag.Flag] -> [String] -> Either String Config.Config
    configured fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags fs
    key = replicate 63 '0' <> "7"
