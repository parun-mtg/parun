-- | The functions the page calls; a reactor module has no main.
module Exports where

import qualified Parun.Browser.Shell

foreign export javascript "start" start :: IO ()

-- | The web client: its home screen, or the scenario the page names.
start :: IO ()
start = Parun.Browser.Shell.start
