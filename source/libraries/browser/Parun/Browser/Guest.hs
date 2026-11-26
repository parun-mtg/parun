-- | A seat of another browser's game, played on this page (#9185): no engine
-- runs here, only the requests the host sends the seat, each answered on the
-- page as the hotseat answers its own (Hotseat.exchange), the page naming
-- the seat the host says decides meanwhile and showing the clocks it sends,
-- until the host says how the game ended.
module Parun.Browser.Guest where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified GHC.Clock as Clock
import qualified Parun.Browser as Browser
import qualified Parun.Browser.Hotseat as Hotseat
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Request as Request
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Web.Clock as Web.Clock
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Link as Link
import qualified Parun.Web.Question as Web.Question
import qualified Parun.Web.Screen as Screen

-- | The game as seat `seat`, over `channel`, data channel `link`, to the
-- host: each request answered on the page at the auto-pass level kept in
-- `levelRef`, given up should the host say anything first (the game ended
-- as the seat's time ran out, say), a card tapped showing its text through
-- `lookUp`, the home screen through `leave`, a concession that decides the
-- game sent to the host at once, the log starting from what the
-- host says the seat has `seen`, the clocks running as the host last said;
-- how it came out. Once the channel closes, `rejoin` connects it anew, and
-- the game goes on as the host asks again; it stops where `rejoin` does not
-- connect.
play :: Browser.Page Screen.Event -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> IORef.IORef Web.Question.Level -> Link.Channel IO -> Int -> IO Bool -> PlayerId.PlayerId -> Ending.Seen -> (Maybe PlayerView.PlayerView -> IO Hotseat.Leave) -> IO Ending.Played
play page lookUp levelRef channel link rejoin seat seen leave = do
  seatRef <- IORef.newIORef . Hotseat.newSeat seat seen =<< IORef.readIORef levelRef
  Hotseat.keepingLevel levelRef seatRef . Exception.handle (\Hotseat.MkAbandonedGame -> pure Ending.MkAbandoned) $ do
    -- The seat the host last said decides, until the guest's own is asked.
    decidingRef <- IORef.newIORef Nothing
    let -- A concession that leaves at once (Web.Question.MkConceded) is
        -- the host's to take the seat out with.
        concede _ = Link.send channel Link.MkConcede
        -- One the guest's own question asks leaves at once (the host
        -- rewinds where the seat has priority, Command.rewinding).
        conceded seat = True <$ concede seat
        -- The clocks as the host says they read, the seat's own running
        -- until it answers.
        clocked deciding banks = Clock.getMonotonicTime >>= \at -> IORef.modifyIORef' seatRef (\s -> s {Hotseat.clocks = Web.Clock.heard at deciding banks})
        local banks request = do
          IORef.writeIORef decidingRef Nothing
          clocked seat banks
          answered <- Browser.heeding page link (Hotseat.exchange lookUp page seatRef leave conceded request)
          at <- Clock.getMonotonicTime
          IORef.modifyIORef' seatRef (\s -> s {Hotseat.clocks = Web.Clock.stop at (Hotseat.clocks s)})
          pure (Monad.join answered)
        -- While the host's game goes on, the page stays live, waiting on
        -- that seat, or every other before the host names one.
        others state = [SeatView.player s | request <- Maybe.maybeToList (Hotseat.current state), shown <- Maybe.maybeToList (Request.view request), s <- PlayerView.seats shown, SeatView.player s /= seat]
        loop = do
          deciding <- IORef.readIORef decidingRef
          heard <- Hotseat.awaiting lookUp page seatRef leave concede (\state -> maybe (others state) pure deciding) (Link.receive channel)
          case heard of
            Nothing -> do
              back <- rejoin
              if back then loop else Ending.endingOf Ending.MkStopped [Text.pack "The game stopped: the connection to the host closed."] Text.empty seat False . Hotseat.sighted <$> IORef.readIORef seatRef
            Just message -> do
              answered <- Link.guest local message
              case answered of
                Link.MkReply reply -> Link.send channel reply >> loop
                Link.MkWaitingOn other shown depth banks -> IORef.writeIORef decidingRef (Just other) >> clocked other banks >> IORef.modifyIORef' seatRef (Hotseat.stand seat shown depth) >> loop
                -- The host's, which holds what this page did not see: the
                -- killing blow, and the game before a reload.
                Link.MkEnded ending -> pure ending
                Link.MkIgnored -> loop
    Ending.MkFinished <$> loop
