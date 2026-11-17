-- | The lobby, read from the data the page mounted at Cabal's data directory:
-- the cards from their bundle, the decks from their directory.
module Parun.Browser.Lobby where

import qualified Data.ByteString as ByteString
import qualified Data.Sequence as Seq
import qualified Parun.Browser as Browser
import qualified Parun.Decks as Decks
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Registry as Registry
import qualified Parun.Web.App as App
import qualified Parun.Web.Lobby as Lobby

start :: IO ()
start = do
  root <- Registry.defaultRoot
  loaded <- fmap (Registry.loadBundle root) $ Registry.bundlePath >>= ByteString.readFile
  deckRoot <- Decks.defaultRoot
  paths <- Decks.listRoot deckRoot
  Browser.run app . Lobby.MkLobby (Natural.length loaded) . Seq.fromList $ fmap (Lobby.deckName deckRoot) paths

app :: App.App Lobby.Lobby ()
app = App.MkApp {App.update = const id, App.view = Lobby.view}
