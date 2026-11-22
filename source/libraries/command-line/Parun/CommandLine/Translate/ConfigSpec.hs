module Parun.CommandLine.Translate.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Parun.CommandLine.Translate.Config as Config
import qualified Parun.CommandLine.Translate.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Translate.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads the two files, then the names, in order" $
      Spec.assertEq s (configured [] ["m.json", "c.json", "Shock", "@names.txt"]) (Right Config.MkConfig {Config.sources = Just ("m.json", "c.json"), Config.out = Nothing, Config.set = Nothing, Config.names = fmap Text.pack ["Shock", "@names.txt"], Config.help = False})
    Spec.it s "reads where to write and the set" $
      Spec.assertEq s (fmap (\c -> (Config.out c, Config.set c)) $ configured [Flag.Out "dir", Flag.Set "LEA"] ["m.json", "c.json"]) (Right (Just "dir", Just (Text.pack "LEA")))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap Config.help $ configured [Flag.Help True, Flag.Help False] ["m.json", "c.json"]) (Right False)
    Spec.it s "needs no file when asked for the usage" $
      Spec.assertEq s (configured [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing file" $
      Spec.assertEq s (configured [Flag.Set "LEA"] ["m.json"]) (Left "missing argument: CANDIDATES")
  where
    configured :: [Flag.Flag] -> [String] -> Either String Config.Config
    configured fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags fs
