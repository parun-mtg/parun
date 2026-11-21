-- | What a tap on any of the web client's screens means: the page's one
-- event type, each screen's own wrapped.
module Parun.Web.Screen where

import qualified Parun.Web.Builder as Builder
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Home as Home
import qualified Parun.Web.Join as Join
import qualified Parun.Web.Lobby as Lobby
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Rejoin as Rejoin
import qualified Parun.Web.Replay as Web.Replay

data Event
  = MkHome Home.Event
  | MkLobby Lobby.Event
  | MkQuestion Web.Question.Event
  | MkReplay Web.Replay.Event
  | MkEnding Ending.Event
  | MkBuilder Builder.Event
  | MkJoin Join.Event
  | MkRejoin Rejoin.Event
  deriving (Eq, Show)
