-- | The web client's last screen: how the game ended for the player, a link
-- saving it as a scenario, one starting another, and everything the player
-- saw happen.
module Parun.Web.Ending where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Numeric.Natural (Natural)
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Uri.Fragment as Fragment
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
  deriving (Eq, Show)

-- | How the game ended for the player the page plays.
data Verdict
  = MkWon
  | MkLost
  | MkDrawn
  | -- | Play stopped before the game ended.
    MkStopped
  deriving (Bounded, Enum, Eq, Show)

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
    -- | The seat the page plays.
    viewer :: PlayerId.PlayerId,
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

view :: Ending -> Html.Html Event
view ending =
  let (headline, tone) = case verdict ending of
        MkWon -> ("You win!", "won")
        MkLost -> ("You lose", "lost")
        MkDrawn -> ("A draw", "drawn")
        MkStopped -> ("Game over", "stopped")
   in Element.el
        "main"
        ["ending"]
        []
        $ [confetti | verdict ending == MkWon]
          <> [ Element.el "section" ["verdict", tone] [] $
                 Element.el "h1" [] [] [Element.string headline] : [Element.el "p" [] [] (Mana.rich line) | line <- said ending],
               Element.el
                 "div"
                 ["ending-actions"]
                 []
                 [ Element.button ["primary"] MkRematch [Element.string "Rematch"],
                   Element.button [] MkNewGame [Element.string "New game"],
                   Element.el "a" [] [Element.attr "href" (download (scenario ending)), Element.attr "download" (Text.pack "parun-game.json")] [Element.string "Save as a scenario"]
                 ],
               Element.el "h2" [] [] [Element.string "How it went"],
               summary (viewer ending) (trail ending),
               Element.el "h2" [] [] [Element.string "Game log"],
               Element.el "ul" ["game-log"] [] (fmap (Log.item (viewer ending) Nothing False) (Foldable.toList (witnessed ending)))
             ]

-- | Each seat's life through the game as a line chart, and the numbers it
-- tells: turns played, and each seat's final and lowest life and biggest
-- single loss.
summary :: PlayerId.PlayerId -> [(Natural, [(PlayerId.PlayerId, Integer)])] -> Html.Html msg
summary me samples =
  let seats = ListUtils.nubOrd (concatMap (fmap fst . snd) samples)
      lifeOf pid sample = Maybe.fromMaybe 0 (lookup pid (snd sample))
      lives = [lifeOf pid sample | sample <- samples, pid <- seats]
      lo = minimum (0 : lives)
      hi = maximum (20 : lives)
      width = 320 :: Integer
      height = 120 :: Integer
      count = length samples
      x i = if count <= 1 then 0 else div (toInteger i * width) (toInteger (count - 1))
      y life = height - div ((life - lo) * height) (max 1 (hi - lo))
      name pid = if pid == me then Text.pack "You" else Wording.player pid
      -- Each seat its own colour, by its seat; the viewer's line drawn bolder.
      hue pid = ["seat-" <> show (mod (PlayerId.unwrap pid) 6)] <> ["you" | pid == me]
      line pid =
        Element.el
          "polyline"
          ("life-line" : hue pid)
          [Element.attr "points" (Text.unwords [Text.pack (show (x i) <> "," <> show (y (lifeOf pid sample))) | (i, sample) <- zip [0 :: Int ..] samples]), Element.attr "fill" (Text.pack "none")]
          []
      turns = maybe 0 fst (Maybe.listToMaybe (reverse samples))
      drops pid = [lifeOf pid a - lifeOf pid b | (a, b) <- zip samples (drop 1 samples)]
      stat pid =
        Element.el
          "li"
          (hue pid)
          []
          [ Element.el "strong" [] [] [Element.text (name pid)],
            Element.text (Text.pack (": ended on " <> show (maybe 0 (lifeOf pid) (Maybe.listToMaybe (reverse samples))) <> " life, lowest " <> show (maybe 0 minimum (NonEmpty.nonEmpty (fmap (lifeOf pid) samples))) <> ", biggest hit " <> show (maximum (0 : drops pid))))
          ]
   in Element.el
        "section"
        ["summary"]
        []
        [ Element.el
            "svg"
            ["life-chart"]
            [Element.attr "viewBox" (Text.pack ("-4 -4 " <> show (width + 8) <> " " <> show (height + 8))), Element.attr "role" (Text.pack "img"), Element.attr "aria-label" (Text.pack "Life totals over the game")]
            (Element.el "line" ["zero"] [Element.attr "x1" (Text.pack "0"), Element.attr "x2" (Text.pack (show width)), Element.attr "y1" (Text.pack (show (y 0))), Element.attr "y2" (Text.pack (show (y 0)))] [] : fmap line seats),
          Element.el "p" ["chart-axis"] [] [Element.el "span" [] [] [Element.string "Start"], Element.el "span" [] [] [Element.string ("Turn " <> show turns)]],
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

-- | A data URL holding the text as JSON (RFC 2397).
download :: Text.Text -> Text.Text
download contents =
  Text.pack "data:application/json;charset=utf-8,"
    <> Encoding.decodeLatin1 (LazyByteString.toStrict . Builder.toLazyByteString . Fragment.encode $ Encoding.encodeUtf8Builder contents)
