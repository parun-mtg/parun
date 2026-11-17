-- | The functions the page calls; a reactor module has no main.
module Exports where

import qualified Parun.Browser.Lobby

foreign export javascript "start" start :: IO ()

start :: IO ()
start = Parun.Browser.Lobby.start
