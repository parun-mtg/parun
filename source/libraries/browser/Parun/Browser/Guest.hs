-- | A seat of another browser's game, played on this page (#9185): no engine
-- runs here, only the requests the host sends the seat, each answered on the
-- page as the hotseat answers its own (Hotseat.exchange), until the host
-- says how the game ended.
module Parun.Browser.Guest where

import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.Text as Text
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Screen as Screen

-- | The game as seat `seat`, over `channel` to the host: each request
-- answered on the page at the auto-pass level kept in `levelRef`, a card
-- tapped showing its text through `lookUp`, the home screen through
-- `leave`; how it came out. Once the channel closes, `rejoin` connects it
-- anew, and the game goes on as the host asks again; it stops where `rejoin`
-- does not connect.
play :: Browser.Page Screen.Event -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> IORef.IORef Web.Question.Level -> Link.Channel IO -> IO Bool -> PlayerId.PlayerId -> (Maybe PlayerView.PlayerView -> IO Hotseat.Leave) -> IO Ending.Played
play page lookUp levelRef channel rejoin seat leave = do
  seatRef <- IORef.newIORef . Hotseat.newSeat seat =<< IORef.readIORef levelRef
  Hotseat.keepingLevel levelRef seatRef . Exception.handle (\Hotseat.MkAbandonedGame -> pure Ending.MkAbandoned) $ do
    let local = Hotseat.exchange lookUp page seatRef leave
        loop = do
          heard <- Link.receive channel
          case heard of
            Nothing -> do
              back <- rejoin
              if back then loop else pure (Ending.MkStopped, [Text.pack "The game stopped: the connection to the host closed."], Text.empty)
            Just message -> do
              answered <- Link.guest local message
              case answered of
                Link.MkReply reply -> Link.send channel reply >> loop
                Link.MkEnded verdict said scenario -> pure (verdict, said, scenario)
                Link.MkIgnored -> loop
    (verdict, said, scenario) <- loop
    finished <- IORef.readIORef seatRef
    pure (Ending.MkFinished (Ending.MkEnding verdict said scenario (Hotseat.loggedFor seat finished) seat (Foldable.toList (Hotseat.lives finished))))
