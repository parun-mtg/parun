-- | The functions the page calls; a reactor module has no main.
module Exports where

import qualified Parun.Browser.Hotseat
import qualified Parun.Browser.Replay
import qualified System.Environment as Environment

foreign export javascript "start" start :: IO ()

-- | A game, or the scenario the page names (parun_scenario, the page's
-- ?scenario) replayed.
start :: IO ()
start = Environment.lookupEnv "parun_scenario" >>= maybe Parun.Browser.Hotseat.start Parun.Browser.Replay.start
