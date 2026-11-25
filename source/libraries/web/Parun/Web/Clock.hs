-- | The players' clocks (#9730): every seat a person plays has a time bank,
-- set in the lobby, which runs only while the game waits on that seat's
-- decision; a seat whose time runs out loses. Paper Magic has no such
-- clock: it is the web client's, and the engine never sees it.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * A seat out of time in a game of three or more leaving it, the others
--     playing on: the game stops (#9757).
--   * A game picked up after a reload charging the seat deciding then the
--     time it spent on that decision: its clock reads as at the last reply
--     kept (#9780).
module Parun.Web.Clock where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Reply as R
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Ending as Ending
import qualified Parun.Web.Html as Html

-- | Each timed seat's time left, the seat whose clock runs and since when,
-- and the seat whose time ran out; times in seconds of a monotonic clock.
data Clocks = MkClocks
  { left :: Map.Map PlayerId.PlayerId Double,
    running :: Maybe (PlayerId.PlayerId, Double),
    out :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Show)

-- | No seat timed.
none :: Clocks
none = MkClocks Map.empty Nothing Nothing

-- | Each of these seats given so many minutes, or none untimed.
starting :: Maybe Natural -> Set.Set PlayerId.PlayerId -> Clocks
starting minutes seats = none {left = foldMap (\m -> Map.fromSet (const (60 * Natural.toDouble m)) seats) minutes}

-- | The lobby's choices of time per seat, in minutes, each with its value
-- and its label, after none.
choices :: [(Maybe Natural, (String, String))]
choices = (Nothing, ("off", "Off")) : [(Just m, (show m, show m <> " minutes")) | m <- [5, 10, 20, 30, 60]]

-- | The seat's clock started at this time.
start :: PlayerId.PlayerId -> Double -> Clocks -> Clocks
start seat at clocks = clocks {running = Just (seat, at)}

-- | The running clock stopped at this time, its seat charged the time it
-- ran, never below none left.
stop :: Double -> Clocks -> Clocks
stop at clocks = case running clocks of
  Nothing -> clocks
  Just (seat, since) -> clocks {left = Map.adjust (\l -> max 0 (l - (at - since))) seat (left clocks), running = Nothing}

-- | A seat's exchange, `ask`, on its clock where it has one: started as the
-- seat is asked, read and changed through `clocks` and `change`, at the
-- times `now` tells, stopped once it answers; `within` gives up waiting at
-- a deadline, which ends the seat's input, the game stopping
-- (Game.wired), and keeps the seat as the one out of time. RemoteSeatSpec's
-- guest out of time is the proof.
timed :: (Monad m) => m Double -> (Double -> m (Maybe (Either Text.Text R.Reply)) -> m (Maybe (Maybe (Either Text.Text R.Reply)))) -> m Clocks -> ((Clocks -> Clocks) -> m ()) -> PlayerId.PlayerId -> Protocol.Exchange m -> Protocol.Exchange m
timed now within clocks change seat ask request = do
  begun <- now
  bank <- Map.lookup seat . left <$> clocks
  case bank of
    Nothing -> ask request
    Just l -> do
      change (start seat begun)
      answered <- within (begun + l) (ask request)
      ended <- now
      change (stop ended)
      maybe (change (\c -> c {out = Just seat}) >> pure Nothing) pure answered

-- | The clocks of a game picked up after a reload (Parun.Web.Link.Saved),
-- from `fresh`, those it started with, and the time banks kept with each
-- reply made: every seat's time left as the last reply found it.
resumed :: Clocks -> [Map.Map PlayerId.PlayerId Double] -> Clocks
resumed fresh kept = maybe fresh (\l -> fresh {left = l}) (Maybe.listToMaybe (reverse kept))

-- | Each timed seat's time left at this time, in seconds, the running
-- clock read down to it: what a guest's page is sent (Parun.Web.Link).
banks :: Double -> Clocks -> Map.Map PlayerId.PlayerId Double
banks at clocks = Map.mapWithKey (\s l -> max 0 (l - maybe 0 (\(r, since) -> if r == s then at - since else 0) (running clocks))) (left clocks)

-- | The clocks a guest's page keeps from the time banks it was sent as
-- `seat` decides, that seat's clock, if it has one, running from this time.
heard :: Double -> PlayerId.PlayerId -> Map.Map PlayerId.PlayerId Double -> Clocks
heard at seat sent = none {left = sent, running = if Map.member seat sent then Just (seat, at) else Nothing}

-- | A seat's clock as a screen shows it: its time left, and, while it
-- runs, when it runs out, in milliseconds since the Unix epoch, which the
-- page counts down to (page.js).
data Face = MkFace
  { owner :: PlayerId.PlayerId,
    remaining :: Double,
    ends :: Maybe Integer
  }
  deriving (Eq, Show)

-- | Every timed seat's clock at this time of the monotonic clock, which is
-- this many milliseconds since the Unix epoch.
faces :: Double -> Integer -> Clocks -> [Face]
faces at epoch clocks =
  [ MkFace s l (if fmap fst (running clocks) == Just s then Just (epoch + round (l * 1000)) else Nothing)
  | (s, l) <- Map.toList (banks at clocks)
  ]

-- | Time left as minutes and seconds, a part second counted whole, so a
-- clock shows 0:00 only once out.
shownTime :: Double -> String
shownTime seconds =
  let whole = ceiling (max 0 seconds) :: Integer
      (m, sec) = divMod whole 60
   in show m <> ":" <> (if sec < 10 then "0" else "") <> show sec

-- | The clocks on the turn bar of a screen showing `viewer`'s board, its
-- own called "You" unless the screen names its seat (Web.Question.naming).
view :: PlayerId.PlayerId -> Bool -> [Face] -> [Html.Html msg]
view viewer naming shown =
  [ Element.el
      "span"
      (["clock"] <> ["running" | Maybe.isJust (ends face)] <> ["low" | remaining face < 60])
      ([Element.attr "role" (Text.pack "timer"), Element.attr "title" (if yours then Text.pack "Your clock" else who <> Text.pack "'s clock")] <> foldMap (\e -> [Element.attr "data-ends" (Text.pack (show e))]) (ends face))
      [Element.el "span" ["clock-who"] [] [Html.text who], Element.el "span" ["clock-time"] [] [Element.string (shownTime (remaining face))]]
  | face <- shown,
    let yours = owner face == viewer && not naming
        who = if yours then Text.pack "You" else Wording.player (owner face)
  ]

-- | How a game this seat's time ran out in ended for `reader`, of so many
-- seats, or for no seat in particular: lost for the seat out of time, won
-- for the other of two, and stopped for the rest; and what is said of it.
ending :: Natural -> PlayerId.PlayerId -> Maybe PlayerId.PlayerId -> (Ending.Verdict, [Text.Text])
ending count late reader =
  let others = [p | n <- [1 .. count], let p = Command.idOf n, p /= late]
      ran = Wording.player late <> Text.pack " ran out of time."
   in case (others, reader) of
        (_, Just r) | r == late -> (Ending.MkLost, [if count == 2 then ran else Text.pack "The game stopped: " <> ran])
        ([winner], Nothing) -> (Ending.MkWonBy winner, [ran])
        ([_], Just _) -> (Ending.MkWon, [ran])
        _ -> (Ending.MkStopped, [Text.pack "The game stopped: " <> ran])
