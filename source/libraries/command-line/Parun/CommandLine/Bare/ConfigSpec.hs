module Parun.CommandLine.Bare.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import Numeric.Natural (Natural)
import qualified Parun.CommandLine.Bare.Config as Config
import qualified Parun.CommandLine.Bare.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Bare.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads the positional arguments" $
      Spec.assertEq s (configured ["FILE"] (Just 1) [] ["cards.json"]) (Right Config.MkConfig {Config.arguments = ["cards.json"], Config.help = False})
    Spec.it s "sets the switch, a later flag undoing an earlier one" $
      Spec.assertEq s (fmap (fmap Config.help) [configured [] (Just 0) [Flag.Help True] [], configured [] (Just 0) [Flag.Help True, Flag.Help False] []]) [Right True, Right False]
    Spec.it s "needs no argument when asked for the usage" $
      Spec.assertEq s (configured ["FILE"] (Just 1) [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing argument" $
      Spec.assertEq s (configured ["FILE"] (Just 1) [] []) (Left "missing argument: FILE")
    Spec.it s "refuses an extra argument" $
      Spec.assertEq s (configured [] (Just 0) [] ["x"]) (Left "unexpected argument: x")
  where
    configured :: [String] -> Maybe Natural -> [Flag.Flag] -> [String] -> Either String Config.Config
    configured required most fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags required most fs
