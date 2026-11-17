-- | The lobby, read from the data the page mounted at Cabal's data directory.
module Parun.Browser.Lobby where

import qualified Data.Sequence as Seq
import qualified Parun.Browser as Browser
import qualified Parun.Decks as Decks
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Registry as Registry
import qualified Parun.Web.App as App
import qualified Parun.Web.Lobby as Lobby

start :: IO ()
start = do
  loaded <- Registry.defaultRoot >>= Registry.loadRoot
  root <- Decks.defaultRoot
  paths <- Decks.listRoot root
  Browser.run app . Lobby.MkLobby (Natural.length loaded) . Seq.fromList $ fmap (Lobby.deckName root) paths

app :: App.App Lobby.Lobby ()
app = App.MkApp {App.update = const id, App.view = Lobby.view}
