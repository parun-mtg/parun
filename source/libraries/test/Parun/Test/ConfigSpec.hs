module Parun.Test.ConfigSpec where

import qualified Control.Exception as Exception
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Parun.Play.Key as Key
import qualified Parun.Spec as Spec
import qualified Parun.Test.Config as Config
import qualified Parun.Test.Flag as Flag

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Test.Config" $ do
  Spec.describe s "fromFlags" $ do
    Spec.it s "is initial when given nothing" $
      Spec.assertEq s (configured []) (Right Config.MkConfig {Config.patterns = [], Config.timeout = Nothing, Config.seed = Nothing, Config.json = Nothing, Config.list = False, Config.failFast = False, Config.help = False})
    Spec.it s "collects every pattern, in order" $
      Spec.assertEq s (fmap Config.patterns $ configured [Flag.Pattern "a", Flag.Pattern "b c"]) (Right [Text.pack "a", Text.pack "b c"])
    Spec.it s "reads the CI's timeout" $
      Spec.assertEq s (fmap Config.timeout $ configured [Flag.Timeout "120s"]) (Right (Just 120000000))
    Spec.it s "reads a seed" $
      Spec.assertEq s (fmap Config.seed $ configured [Flag.Seed (replicate 63 '0' <> "7")]) (Right (Key.fromHex . Text.pack $ replicate 63 '0' <> "7"))
    Spec.it s "reads where to write the report" $
      Spec.assertEq s (fmap Config.json $ configured [Flag.Json "report.json"]) (Right (Just "report.json"))
    Spec.it s "sets each switch" $
      Spec.assertEq s (fmap switches $ configured [Flag.List True, Flag.FailFast True, Flag.Help True]) (Right (True, True, True))
    Spec.it s "lets a later flag undo an earlier one" $
      Spec.assertEq s (fmap switches $ configured [Flag.List True, Flag.FailFast True, Flag.Help True, Flag.List False, Flag.FailFast False, Flag.Help False]) (Right (False, False, False))
    Spec.it s "refuses a seed that is not a key" $
      Spec.assertEq s (configured [Flag.Seed "x"]) (Left "not a seed: x")
    Spec.it s "refuses a duration it cannot read" $
      Spec.assertEq s (configured [Flag.Timeout "2d"]) (Left "not a duration: 2d")

  Spec.describe s "duration" $ do
    Spec.it s "reads bare seconds" $
      Spec.assertEq s (Config.duration "3") (Just 3000000)
    Spec.it s "reads each unit" $
      Spec.assertEq s (fmap Config.duration ["500ms", "2s", "2m", "1h"]) [Just 500000, Just 2000000, Just 120000000, Just 3600000000]
    Spec.it s "refuses a unit alone, and an unknown unit" $
      Spec.assertEq s (fmap Config.duration ["", "s", "2d"]) [Nothing, Nothing, Nothing]
  where
    configured :: [Flag.Flag] -> Either String Config.Config
    configured = Bifunctor.first (\(Exception.SomeException e) -> Exception.displayException e) . Config.fromFlags
    switches c = (Config.list c, Config.failFast c, Config.help c)
