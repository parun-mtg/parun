-- | The functions the page calls; a reactor module has no main.
module Exports where

import qualified Parun.Browser.Hotseat

foreign export javascript "start" start :: IO ()

start :: IO ()
start = Parun.Browser.Hotseat.start
