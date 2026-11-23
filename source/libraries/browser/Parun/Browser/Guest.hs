-- | A seat of another browser's game, played on this page (#9185): no engine
-- runs here, only the requests the host sends the seat, each answered on the
-- page as the hotseat answers its own (Hotseat.exchange), the page naming
-- the seat the host says decides meanwhile, until the host says how the game
-- ended.
module Parun.Browser.Guest where

import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Request as Request
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Screen as Screen

-- | The game as seat `seat`, over `channel` to the host: each request
-- answered on the page at the auto-pass level kept in `levelRef`, a card
-- tapped showing its text through `lookUp`, the home screen through
-- `leave`, the log starting from what the host says the seat has `seen`;
-- how it came out. Once the channel closes, `rejoin` connects it
-- anew, and the game goes on as the host asks again; it stops where `rejoin`
-- does not connect.
play :: Browser.Page Screen.Event -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> IORef.IORef Web.Question.Level -> Link.Channel IO -> IO Bool -> PlayerId.PlayerId -> Ending.Seen -> (Maybe PlayerView.PlayerView -> IO Hotseat.Leave) -> IO Ending.Played
play page lookUp levelRef channel rejoin seat seen leave = do
  seatRef <- IORef.newIORef . Hotseat.newSeat seat seen =<< IORef.readIORef levelRef
  Hotseat.keepingLevel levelRef seatRef . Exception.handle (\Hotseat.MkAbandonedGame -> pure Ending.MkAbandoned) $ do
    -- The seat the host last said decides, until the guest's own is asked.
    decidingRef <- IORef.newIORef Nothing
    let local request = IORef.writeIORef decidingRef Nothing >> Hotseat.exchange lookUp page seatRef leave request
        -- While the host's game goes on, the page stays live, waiting on
        -- that seat, or every other before the host names one.
        others state = [SeatView.player s | request <- Maybe.maybeToList (Hotseat.current state), shown <- Maybe.maybeToList (Request.view request), s <- PlayerView.seats shown, SeatView.player s /= seat]
        loop = do
          deciding <- IORef.readIORef decidingRef
          heard <- Hotseat.awaiting lookUp page seatRef leave (\state -> maybe (others state) pure deciding) (Link.receive channel)
          case heard of
            Nothing -> do
              back <- rejoin
              if back then loop else Ending.endingOf Ending.MkStopped [Text.pack "The game stopped: the connection to the host closed."] Text.empty seat False . Hotseat.sighted <$> IORef.readIORef seatRef
            Just message -> do
              answered <- Link.guest local message
              case answered of
                Link.MkReply reply -> Link.send channel reply >> loop
                Link.MkWaitingOn other shown depth -> IORef.writeIORef decidingRef (Just other) >> IORef.modifyIORef' seatRef (Hotseat.stand seat shown depth) >> loop
                -- The host's, which holds what this page did not see: the
                -- killing blow, and the game before a reload.
                Link.MkEnded ending -> pure ending
                Link.MkIgnored -> loop
    Ending.MkFinished <$> loop
