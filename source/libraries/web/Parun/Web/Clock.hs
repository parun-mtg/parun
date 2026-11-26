-- | The players' clocks (#9730): every seat a person plays has a time bank,
-- set in the lobby, which runs only while the game waits on that seat's
-- decision; a seat whose time runs out loses. Paper Magic has no such
-- clock: it is the web client's, and the engine never sees it.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * A seat out of time in a game of three or more at a question other
--     than its priority leaving it, the others playing on: the game stops
--     (#9646).
module Parun.Web.Clock where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Element as Element
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
-- a deadline, the request going to `expire`, which takes the seat out of a
-- game playing on where it can (Parun.Play.Command.MkLeaving), else ends
-- the seat's input, the game stopping (Game.wired), and keeps the seat as
-- the one out of time. RemoteSeatSpec's guest out of time is the proof,
-- and its seat out of time at its priority in a game of three.
timed :: (Monad m) => m Double -> (Double -> m (Maybe (Either Text.Text R.Reply)) -> m (Maybe (Maybe (Either Text.Text R.Reply)))) -> m Clocks -> ((Clocks -> Clocks) -> m ()) -> (Request.Request -> m ()) -> PlayerId.PlayerId -> Protocol.Exchange m -> Protocol.Exchange m
timed now within clocks change expire seat ask request = do
  begun <- now
  bank <- Map.lookup seat . left <$> clocks
  case bank of
    Nothing -> ask request
    Just l -> do
      change (start seat begun)
      answered <- within (begun + l) (ask request)
      ended <- now
      change (stop ended)
      maybe (expire request >> change (\c -> c {out = Just seat}) >> pure Nothing) pure answered

-- | The running clock as the page keeps it for a reload
-- (Parun.Web.Link.savedRunning): its seat, that seat's time left as it
-- started, in seconds, and when it started and when the page was left, if
-- it was, in milliseconds since the Unix epoch.
data Kept = MkKept
  { deciding :: PlayerId.PlayerId,
    bankThen :: Double,
    startedAt :: Integer,
    leftAt :: Maybe Integer
  }
  deriving (Eq, Show)

-- | The running clock at this time of the monotonic clock, which is this
-- many milliseconds since the Unix epoch, as the page keeps it.
keeping :: Double -> Integer -> Clocks -> Maybe Kept
keeping at epoch clocks = do
  (seat, since) <- running clocks
  bank <- Map.lookup seat (left clocks)
  pure (MkKept seat bank (epoch - round ((at - since) * 1000)) Nothing)

-- | The clocks of a game picked up after a reload (Parun.Web.Link.Saved),
-- from `fresh`, those it started with, and the time banks kept with each
-- reply made: every seat's time left as the last reply found it, but the
-- seat whose clock ran as the page was left, if one did, charged from its
-- time left as it started the time it ran until then, never below none. A
-- clock kept unstamped (the page crashed, say) charges none of its run.
resumed :: Clocks -> [Map.Map PlayerId.PlayerId Double] -> Maybe Kept -> Clocks
resumed fresh kept pending =
  let replied = maybe fresh (\l -> fresh {left = l}) (Maybe.listToMaybe (reverse kept))
      charged k = max 0 (bankThen k - maybe 0 (\l -> Natural.toDouble (Integer.toNaturalSaturating (l - startedAt k)) / 1000) (leftAt k))
   in maybe replied (\k -> replied {left = Map.adjust (const (charged k)) (deciding k) (left replied)}) pending

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
