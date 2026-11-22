module Parun.CommandLine.Deck.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Parun.CommandLine.Deck.Config as Config
import qualified Parun.CommandLine.Deck.Flag as Flag
import qualified Parun.Spec as Spec

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.CommandLine.Deck.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "reads the file alone" $
      Spec.assertEq s (configured [] ["d.txt"]) (Right Config.MkConfig {Config.format = Nothing, Config.pool = Nothing, Config.file = Just "d.txt", Config.help = False})
    Spec.it s "reads the format and the pool" $
      Spec.assertEq s (fmap (\c -> (Config.format c, Config.pool c)) $ configured [Flag.Pool "p.txt", Flag.Format "sealed"] ["d.txt"]) (Right (Just (Text.pack "sealed"), Just "p.txt"))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap Config.help $ configured [Flag.Help True, Flag.Help False] ["d.txt"]) (Right False)
    Spec.it s "needs no file when asked for the usage" $
      Spec.assertEq s (configured [Flag.Help True] []) (Right Config.initial {Config.help = True})
    Spec.it s "refuses a missing file" $
      Spec.assertEq s (configured [] []) (Left "missing argument: FILE")
    Spec.it s "refuses a second file" $
      Spec.assertEq s (configured [] ["d.txt", "e.txt"]) (Left "unexpected argument: e.txt")
  where
    configured :: [Flag.Flag] -> [String] -> Either String Config.Config
    configured fs = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags fs
