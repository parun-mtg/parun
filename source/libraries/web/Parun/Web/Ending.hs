-- | The web client's last screen: how the game ended for the player, its
-- replay, another game, and everything the player saw happen.
module Parun.Web.Ending where

import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.PlayerId as Codec.PlayerId
import qualified Parun.Codec.TeamId as Codec.TeamId
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Teams as Teams
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Mana as Mana

-- | What a tap on the ending does.
data Event
  = -- | The same decks again, shuffled afresh.
    MkRematch
  | -- | Back to the home screen.
    MkNewGame
  | -- | This game's replay, from its start.
    MkWatchReplay
  deriving (Eq, Show)

-- | How the game ended for the player the page plays, or who won where
-- more than one person played at its screen.
data Verdict
  = MkWon
  | MkLost
  | MkDrawn
  | -- | Won by this seat, at a screen more than one person played (shared).
    MkWonBy PlayerId.PlayerId
  | -- | Won by this team (CR 104.2c), at a screen more than one person
    -- played (shared).
    MkTeamWonBy TeamId.TeamId
  | -- | Play stopped before the game ended.
    MkStopped
  deriving (Eq, Show)

-- | A verdict: a bare tag, or a seat's or a team's win at a shared screen
-- as @{"MkWonBy": seat}@ or @{"MkTeamWonBy": team}@.
verdictCodec :: Codec.Codec Verdict
verdictCodec =
  Arm.keyed
    tagOf
    [ Arm.nullary "MkWon" MkWon,
      Arm.nullary "MkLost" MkLost,
      Arm.nullary "MkDrawn" MkDrawn,
      Arm.payload "MkWonBy" Codec.PlayerId.codec MkWonBy (\v -> case v of MkWonBy pid -> Just pid; _ -> Nothing),
      Arm.payload "MkTeamWonBy" Codec.TeamId.codec MkTeamWonBy (\v -> case v of MkTeamWonBy team -> Just team; _ -> Nothing),
      Arm.nullary "MkStopped" MkStopped
    ]
  where
    tagOf v = case v of
      MkWon -> "MkWon"
      MkLost -> "MkLost"
      MkDrawn -> "MkDrawn"
      MkWonBy _ -> "MkWonBy"
      MkTeamWonBy _ -> "MkTeamWonBy"
      MkStopped -> "MkStopped"

-- | How the game ended for this seat, or for no seat in particular: a
-- team's win is each member's (CR 104.2c).
verdictOf :: Maybe PlayerId.PlayerId -> Either a (Result.Result, GameState.GameState) -> Verdict
verdictOf told outcome = case outcome of
  Left _ -> MkStopped
  Right (Result.Won pid, _) -> maybe (MkWonBy pid) (\seat -> if pid == seat then MkWon else MkLost) told
  Right (Result.TeamWon team, gs) -> maybe (MkTeamWonBy team) (\seat -> if Teams.teamOf (GameSettings.teams (MatchScope.settings (GameState.match gs))) seat == Just team then MkWon else MkLost) told
  Right (Result.Drawn, _) -> MkDrawn

-- | How a game ended for `told`, of so many seats, or for no seat in
-- particular, as seat `leaver` left it before it was over, for the reason
-- `why` gives (a clock run out, Parun.Web.Clock; a concession that decides
-- the game, Parun.Web.Question.decisive): lost for that seat, won for the
-- other of two, and stopped for the rest; and what is said of it.
leftEarly :: Natural -> Text.Text -> PlayerId.PlayerId -> Maybe PlayerId.PlayerId -> (Verdict, [Text.Text])
leftEarly count why leaver told =
  let others = [p | n <- [1 .. count], let p = Command.idOf n, p /= leaver]
      left = Wording.player leaver <> Text.pack " " <> why <> Text.pack "."
   in case (others, told) of
        (_, Just r) | r == leaver -> (MkLost, [if count == 2 then left else Text.pack "The game stopped: " <> left])
        ([winner], Nothing) -> (MkWonBy winner, [left])
        ([_], Just _) -> (MkWon, [left])
        _ -> (MkStopped, [Text.pack "The game stopped: " <> left])

-- | How a game the page played came out.
data Played
  = MkFinished Ending
  | -- | Given up from the home screen for another game, so no ending shows.
    MkAbandoned
  deriving (Eq, Show)

data Ending = MkEnding
  { verdict :: Verdict,
    -- | The result and how each player left (Wording.result,
    -- Wording.departures), or why play stopped first.
    said :: [Text.Text],
    -- | The game as a scenario file (Command.rendered).
    scenario :: Text.Text,
    -- | What the player saw happen, oldest first (Log.unseen).
    witnessed :: Seq.Seq Log.Entry,
    -- | The seat the page plays, whose log this is.
    viewer :: PlayerId.PlayerId,
    -- | More than one person played at this screen, so the ending takes no
    -- seat's side: it names every seat, the viewer's too.
    shared :: Bool,
    -- | Each seat's life as the game went, a sample whenever it or the turn
    -- changed (sampled), each beside the turn it was in.
    trail :: [(Natural, [(PlayerId.PlayerId, Integer)])]
  }
  deriving (Eq, Show)

-- | The trail with each seat's life at this turn added, where it or the
-- turn changed since the last sample, so the trail ends on the turn last
-- seen though no life changed in it.
sampled :: Natural -> [(PlayerId.PlayerId, Integer)] -> Seq.Seq (Natural, [(PlayerId.PlayerId, Integer)]) -> Seq.Seq (Natural, [(PlayerId.PlayerId, Integer)])
sampled turn now trail' = if Seq.lookup (Seq.length trail' - 1) trail' == Just (turn, now) then trail' else trail' Seq.|> (turn, now)

-- | What a page has seen of a game, from the views its seats were shown.
data Seen = MkSeen
  { -- | How far each seat has read each game's log (Log.unseen).
    marks :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural),
    -- | The entries each seat has read so far, oldest first.
    logged :: Map.Map PlayerId.PlayerId (Seq.Seq Log.Entry),
    -- | Each seat's life whenever it or the turn changed (sampled).
    sampledLives :: Seq.Seq (Natural, [(PlayerId.PlayerId, Integer)])
  }
  deriving (Eq, Show)

-- | Nothing seen yet.
unseen :: Seen
unseen = MkSeen Map.empty Map.empty Seq.empty

-- | What was seen, with the entries of the view its seat has not read yet
-- logged for that seat, and each seat's life sampled.
see :: PlayerView.PlayerView -> Seen -> Seen
see shown seen =
  let (moved, unread) = Log.unseen (marks seen) shown
      now = [(SeatView.player seat, SeatView.life seat) | seat <- PlayerView.seats shown]
   in MkSeen moved (Map.alter (Just . flip Log.appended unread . Maybe.fromMaybe Seq.empty) (PlayerView.viewer shown) (logged seen)) (sampled (PlayerView.turn shown) now (sampledLives seen))

-- | The entries a seat has read so far, oldest first.
loggedFor :: PlayerId.PlayerId -> Seen -> Seq.Seq Log.Entry
loggedFor seat seen = Map.findWithDefault Seq.empty seat (logged seen)

-- | The ending seat `seat` is shown, from what it saw, taking no seat's side
-- where `shared'`.
endingOf :: Verdict -> [Text.Text] -> Text.Text -> PlayerId.PlayerId -> Bool -> Seen -> Ending
endingOf verdict' said' scenario' seat shared' seen = MkEnding verdict' said' scenario' (loggedFor seat seen) seat shared' (Foldable.toList (sampledLives seen))

-- | The seat the ending speaks to as "You": none where shared.
reader :: Ending -> Maybe PlayerId.PlayerId
reader ending = if shared ending then Nothing else Just (viewer ending)

-- | The ending, offering the game's replay where it was kept among the
-- replays (`kept`).
view :: Bool -> Ending -> Html.Html Event
view kept ending =
  let -- The headline says the result, so the lines under it leave it out
      -- (said holds it first), but for why play stopped.
      (headline, tone, told) = case verdict ending of
        MkWon -> ("You win!", "won", drop 1 (said ending))
        MkLost -> ("You lose", "lost", drop 1 (said ending))
        MkDrawn -> ("A draw", "drawn", drop 1 (said ending))
        MkWonBy pid -> (Text.unpack (Wording.player pid) <> " wins!", "won", drop 1 (said ending))
        MkTeamWonBy team -> (Text.unpack (Wording.team team) <> " wins!", "won", drop 1 (said ending))
        MkStopped -> ("Game over", "stopped", said ending)
      won = case verdict ending of
        MkWon -> True
        MkWonBy _ -> True
        MkTeamWonBy _ -> True
        _ -> False
      entries = Seq.mapWithIndex (,) (witnessed ending)
   in Element.el
        "main"
        ["ending"]
        []
        $ [confetti | won]
          <> [ Element.el "section" ["verdict", tone] [] $
                 Element.el "h1" [] [] [Element.string headline] : [Element.el "p" [] [] (Mana.rich line) | line <- told],
               Element.el
                 "div"
                 ["ending-actions"]
                 []
                 ( [ Element.button ["primary"] MkRematch [Element.string "Rematch"],
                     Element.button [] MkNewGame [Element.string "New game"]
                   ]
                     <> [Element.button [] MkWatchReplay [Element.string "Watch replay"] | kept]
                 ),
               Element.el "h2" [] [] [Element.string "How it went"],
               summary (reader ending) (trail ending),
               -- A shared screen's log is the seat the page plays's, so its
               -- "You" is that seat.
               Element.el "h2" [] [] [Element.string (if shared ending then "Game log, as " <> Text.unpack (Wording.player (viewer ending)) <> " saw it" else "Game log")],
               -- Newest first under each turn's heading, as the log reads
               -- during play (Log.grouped).
               Element.el "section" ["game-log"] [] [Log.grouped (viewer ending) (const Nothing) (const False) (Log.newestFirst entries)]
             ]

-- | Each seat's life through the game as a line chart, its scale, its turns
-- and each seat's final life written on it, and the numbers it tells: turns
-- played, and each seat's final and lowest life and biggest single loss;
-- the seat `me` called "You", its line bolder.
summary :: Maybe PlayerId.PlayerId -> [(Natural, [(PlayerId.PlayerId, Integer)])] -> Html.Html msg
summary me samples =
  let seats = ListUtils.nubOrd (concatMap (fmap fst . snd) samples)
      lifeOf pid sample = Maybe.fromMaybe 0 (lookup pid (snd sample))
      lives = [lifeOf pid sample | sample <- samples, pid <- seats]
      lo = minimum (0 : lives)
      hi = maximum (20 : lives)
      width = 320 :: Integer
      height = 120 :: Integer
      -- Room around the plot for the labels: the scale on the left, the
      -- final lives on the right, the turns below.
      margin = 28 :: Integer
      count = length samples
      x i = if count <= 1 then 0 else div (toInteger i * width) (toInteger (count - 1))
      y life = height - div ((life - lo) * height) (max 1 (hi - lo))
      name pid = if Just pid == me then Text.pack "You" else Wording.player pid
      -- Each seat its own colour, by its seat; the viewer's line drawn bolder.
      hue pid = ["seat-" <> show (mod (PlayerId.unwrap pid) 6)] <> ["you" | Just pid == me]
      line pid =
        Element.el
          "polyline"
          ("life-line" : hue pid)
          [Element.attr "points" (Text.unwords [Text.pack (show (x i) <> "," <> show (y (lifeOf pid sample))) | (i, sample) <- zip [0 :: Int ..] samples]), Element.attr "fill" (Text.pack "none")]
          []
      final = Maybe.listToMaybe (reverse samples)
      turns = maybe 0 fst final
      label :: [String] -> (Integer, Integer) -> String -> String -> Html.Html msg
      label cls (lx, ly) anchor said' = Element.el "text" cls [Element.attr "x" (Text.pack (show lx)), Element.attr "y" (Text.pack (show ly)), Element.attr "text-anchor" (Text.pack anchor), Element.attr "dominant-baseline" (Text.pack "middle")] [Element.string said']
      -- The scale: the top, the baseline at 0 life, and the floor where life
      -- went below 0 and its label clears the baseline's.
      scale = [label ["scale"] (-6, y life) "end" (show life) | life <- ListUtils.nubOrd ([hi, 0] <> [lo | y lo - y 0 >= 12])]
      -- Each seat's final life at its line's end, seats ending alike sharing
      -- one, each kept clear of the one above it.
      ends = List.sortOn (y . fst) [(life, [pid | pid <- seats, maybe 0 (lifeOf pid) final == life]) | life <- ListUtils.nubOrd [maybe 0 (lifeOf pid) final | pid <- seats]]
      spread _ [] = []
      spread at ((life, whose) : rest) = let here = max at (y life) in (here, life, whose) : spread (here + 12) rest
      endLabel (ly, life, whose) = label ("value" : case whose of [pid] -> hue pid; _ -> []) (width + 6, ly) "start" (show life)
      -- Between "Start" and the last turn, the turns at a round step that
      -- keeps them to a handful, each where the turn's first sample is, each
      -- clear of its neighbours' labels.
      step = Maybe.fromMaybe 1 (List.find (\s -> div turns s <= 5) (concatMap (\p -> fmap (* p) [1, 2, 5]) (iterate (* 10) 1)))
      starts = [(turn, x i) | (i, turn, before) <- List.zip3 [0 :: Int ..] (fmap fst samples) (Nothing : fmap (Just . fst) samples), Just turn /= before]
      clear _ [] = []
      clear at ((turn, tx) : rest) = if tx >= at && tx <= width - 60 then (turn, tx) : clear (tx + 24) rest else clear at rest
      between = clear 44 [(turn, tx) | (turn, tx) <- starts, turn /= turns, mod turn step == 0]
      turnAxis = [label ["axis"] (0, height + 16) "start" "Start"] <> [label ["axis"] (tx, height + 16) "middle" (show turn) | (turn, tx) <- between] <> [label ["axis"] (width, height + 16) "end" ("Turn " <> show turns)]
      drops pid = [lifeOf pid a - lifeOf pid b | (a, b) <- zip samples (drop 1 samples)]
      stat pid =
        Element.el
          "li"
          (hue pid)
          []
          [ Element.el "strong" [] [] [Element.text (name pid)],
            Element.text (Text.pack (": ended on " <> show (maybe 0 (lifeOf pid) final) <> " life, lowest " <> show (maybe 0 minimum (NonEmpty.nonEmpty (fmap (lifeOf pid) samples))) <> ", biggest hit " <> show (maximum (0 : drops pid))))
          ]
   in Element.el
        "section"
        ["summary"]
        []
        [ Element.el
            "svg"
            ["life-chart"]
            [Element.attr "viewBox" (Text.pack (show (-margin) <> " -8 " <> show (width + 2 * margin) <> " " <> show (height + 34))), Element.attr "role" (Text.pack "img"), Element.attr "aria-label" (Text.pack "Life totals over the game")]
            ( Element.el "line" ["zero"] [Element.attr "x1" (Text.pack "0"), Element.attr "x2" (Text.pack (show width)), Element.attr "y1" (Text.pack (show (y 0))), Element.attr "y2" (Text.pack (show (y 0)))] []
                : fmap line seats
                  <> scale
                  <> fmap endLabel (spread 0 ends)
                  <> turnAxis
            ),
          Element.el "ul" ["stats"] [] (fmap stat seats)
        ]

-- | Confetti for a win: the five colours' symbols bursting at once from the
-- middle of the screen, each thrown its own way, then falling, drifting,
-- swaying and tumbling at its own pace; the same every time.
confetti :: Html.Html msg
confetti =
  let colour n = case n of
        0 -> ("w", Mana.MkWhite)
        1 -> ("u", Mana.MkBlue)
        2 -> ("b", Mana.MkBlack)
        3 -> ("r", Mana.MkRed)
        _ -> ("g", Mana.MkGreen)
      piece i =
        -- Each property its own mix of the piece's number, so none moves in
        -- step with another.
        let r k = let h = i * 7919 + k * 104729 + i * k * 31 in mod (div (h * h) 13 + h) 1000
            (tint, motif) = colour (mod (r 4) 5)
            -- Thrown up and out (style.css turns the two into a place), a fan
            -- from straight left to straight right.
            angle = 180 + div (r 1 * 180) 1000
            reach = 22 + mod (r 10) 32
         in Element.el
              "span"
              ["fg-" <> tint]
              [Element.attr "style" (Text.pack ("--angle:" <> show angle <> "deg;--reach:" <> show reach <> "vmin;--delay:" <> show (div (r 2) 8) <> "ms;--spin:" <> show (r 3 - 500) <> "deg;--size:" <> show (14 + mod (r 5) 14) <> "px;--drift:" <> show (div (r 6) 33 - 15) <> "vw;--fall:" <> show (2800 + r 7 * 2) <> "ms;--sway:" <> show (6 + mod (r 8) 18) <> "px;--flutter:" <> show (450 + mod (r 9) 600) <> "ms"))]
              [Element.el "svg" [] [Element.attr "viewBox" (Text.pack "0 0 32 32")] [Element.el "use" [] [Element.attr "href" (Text.pack "#" <> Mana.motifId motif)] []]]
   in Element.el "div" ["confetti"] [Element.attr "aria-hidden" (Text.pack "true")] (fmap piece [1 .. 72 :: Integer])
