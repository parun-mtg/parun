-- | A scenario replayed on the page (#8821): the table at one position of
-- its timeline as a chosen seat sees it, a scrubber over the positions, the
-- entry the run takes next and the question it answers, the list of
-- scenarios to open, and how a game played on from a position ended.
module Parun.Web.Replay where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Mana as Mana
import qualified Parun.Web.Question as Web.Question

-- | What the replay page sends.
data Event
  = -- | Goes to this position.
    MkGo Natural
  | -- | Goes to the position the scrubber's value names.
    MkScrub
  | -- | Shows the table as this seat sees it.
    MkSeat PlayerId.PlayerId
  | -- | A tap on the table or the question's form.
    MkTable Web.Question.Event
  | -- | Filters the list of scenarios by the text typed.
    MkSearch
  | -- | Opens this scenario.
    MkOpen Text.Text
  | -- | Plays on by hand from the position shown.
    MkBranch
  | -- | Back from a replay to the list of scenarios.
    MkBack
  | -- | Back from a game played on to the replay where it began.
    MkResumeReplay
  deriving (Eq, Show)

-- | One position of a replay, as the page shows it.
data Frame = MkFrame
  { -- | The scenario's file name, less its extension.
    name :: Text.Text,
    description :: Text.Text,
    -- | Each seat, and the name the scenario gives it.
    seats :: [(PlayerId.PlayerId, Text.Text)],
    viewer :: PlayerId.PlayerId,
    -- | How many entries the run has taken: from 0 to `positions`.
    position :: Natural,
    -- | How many entries the timeline holds.
    positions :: Natural,
    -- | The table as the viewer sees it; Nothing where the run failed first.
    board :: Maybe PlayerView.PlayerView,
    -- | CR 730.1a: how many games the board's game is nested inside, as
    -- Request.depth counts them.
    depth :: Natural,
    -- | The entry the run takes next, as the scenario writes it, and its
    -- moment; Nothing at the end.
    entry :: Maybe Text.Text,
    -- | The question the entry answers or is checked at, as its seat is sent it.
    question :: Maybe Request.Request,
    -- | Why the run stopped short of this position.
    failure :: Maybe Text.Text,
    reading :: Maybe Card.Reading
  }

-- | The page for a frame: the table, and beside it the replay's controls.
view :: Frame -> Html.Html Event
view frame =
  let asked = question frame >>= \r -> if Question.seat (Request.question r) == viewer frame then Just r {Request.view = board frame} else Nothing
      marks = maybe readOnly (\r -> mapMarks (Web.Question.cues r Set.empty Nothing)) asked
      logged = foldMap (Seq.fromList . snd . Log.unseen Map.empty) (board frame)
   in Element.el "main" ["game", "replay"] [] $
        foldMap (\b -> [Board.turnBar [] (Web.Question.subgameChip (depth frame) <> [Bar.settingsButton Icon.MkMenu "Menu" Nothing Nothing]) b]) (board frame)
          <> [Element.el "div" ["play-area"] [] (foldMap (\b -> [Board.view marks b]) (board frame))]
          <> foldMap (Board.hand marks) (board frame)
          <> [Web.Question.side (Text.pack "Replay") (controls frame <> stop frame asked) (sheet frame) (fmap (fmap MkTable) (Web.Question.logView (board frame) (viewer frame) Web.Question.blank {Web.Question.logged = logged}))]

-- | Every tile opens its sheet, and no seat may be tapped.
readOnly :: Board.Marks Event
readOnly = Board.MkMarks (\o -> Card.MkCue [] (MkTable (Web.Question.MkRead (ObjectView.object o))) Nothing) (const Nothing)

mapMarks :: Board.Marks Web.Question.Event -> Board.Marks Event
mapMarks marks = Board.MkMarks (fmap mapCue (Board.object marks)) (fmap (fmap mapCue) (Board.seat marks))
  where
    mapCue cue = cue {Card.tap = MkTable (Card.tap cue), Card.peek = fmap (Bifunctor.bimap MkTable MkTable) (Card.peek cue)}

-- | The scenario, the scrubber and its steps, and the seats to view as.
controls :: Frame -> [Html.Html Event]
controls frame =
  let at = position frame
      count = positions frame
      -- Punctuation keys, since page.js takes L for the log.
      step label symbol key target enabled =
        Element.el "button" [] ([Element.attr "type" (Text.pack "button"), Element.attr "aria-label" (Text.pack label), Element.attr "aria-keyshortcuts" (Text.pack key), Element.on "click" (MkGo target)] <> [Element.attr "disabled" Text.empty | not enabled]) [Element.string symbol]
   in [ Element.el "h2" ["replay-title"] [] [Element.text (name frame)],
        Element.el "p" ["replay-description"] [] (Mana.rich (description frame)),
        Element.el
          "div"
          ["scrubber"]
          []
          [ step "First position" "\x23EE" "<" 0 (at > 0),
            step "Previous position" "\x25C0" "," (Natural.minusSaturating at 1) (at > 0),
            Element.el
              "input"
              []
              [ Element.attr "type" (Text.pack "range"),
                Element.attr "min" (Text.pack "0"),
                Element.attr "max" (Text.pack (show count)),
                Element.attr "value" (Text.pack (show at)),
                Element.attr "aria-label" (Text.pack "Position"),
                Element.on "change" MkScrub
              ]
              [],
            step "Next position" "\x25B6" "." (min count (at + 1)) (at < count),
            step "Last position" "\x23ED" ">" count (at < count)
          ],
        Element.el "p" ["position"] [] [Element.string (if at < count then "Before entry " <> show (at + 1) <> " of " <> show count else "After all " <> show count <> " entries")],
        Element.el
          "div"
          ["replay-actions"]
          []
          [ Element.button ["branch"] MkBranch [Element.string "Play on from here"],
            Element.button ["back"] MkBack [Element.string "All scenarios"]
          ],
        Element.el
          "div"
          ["segmented", "seats"]
          [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "View as")]
          [ Element.el "button" ["on" | pid == viewer frame] [Element.attr "type" (Text.pack "button"), Element.attr "title" (label <> Text.pack " is " <> Wording.player pid <> Text.pack " on the table"), Element.attr "aria-pressed" (Text.pack (if pid == viewer frame then "true" else "false")), Element.on "click" (MkSeat pid)] [Element.text label]
          | (pid, label) <- seats frame
          ]
      ]

-- | What the run does at the position: the entry it takes, the question it
-- answers there (its form, for the seat asked), or why it stopped.
stop :: Frame -> Maybe Request.Request -> [Html.Html Event]
stop frame asked =
  [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text problem] | problem <- Maybe.maybeToList (failure frame)]
    <> [Element.el "pre" ["entry"] [] [Element.text e] | e <- Maybe.maybeToList (entry frame)]
    <> case (question frame, asked) of
      (_, Just r) -> fmap (fmap MkTable) (Web.Question.form r Set.empty)
      (Just r, Nothing) -> [Element.el "p" ["waiting"] [] [Element.text (seatName frame (Question.seat (Request.question r)) <> Text.pack " is asked " <> Question.kind (Request.question r) <> Text.pack ".")]]
      (Nothing, Nothing) -> []

seatName :: Frame -> PlayerId.PlayerId -> Text.Text
seatName frame pid = maybe (Wording.player pid) snd (List.find ((== pid) . fst) (seats frame))

-- | The sheet of the card being read.
sheet :: Frame -> Maybe (Html.Html Event)
sheet frame = do
  b <- board frame
  r <- reading frame
  o <- Wording.recalled b (Card.object r)
  pure (Card.sheet b o r [] (MkTable Web.Question.MkClose))

-- | The list of scenarios once fetched: a filter, then the names matching
-- it, those the manifest names first under their themes
-- (Parun.Scenario.Load.parseManifest), then every one, each a button that
-- opens it, the first `shown` of them.
picker :: Text.Text -> Maybe (Map.Map Text.Text [Text.Text], [Text.Text]) -> Html.Html Event
picker typed listed =
  let matches = Text.isInfixOf (Text.toLower typed) . Text.toLower
      shown = 200 :: Int
      links = Element.keyed "ul" ["scenario-list"] [] . fmap (\n -> (n, Element.el "li" [] [] [Element.button [] (MkOpen n) [Element.text n]]))
      top = [Element.el "h1" [] [] [Element.string "Scenarios"]]
   in Element.el "main" ["message", "scenarios"] [] $ case listed of
        Nothing -> top <> [Element.el "p" [] [] [Element.string "Fetching the scenarios\x2026"]]
        Just (themes, names) ->
          let matching = filter matches names
              featured = [Element.el "section" ["theme"] [] [Element.el "h2" [] [] [Element.text theme], links named'] | (theme, named) <- Map.toList themes, let named' = filter matches named, not (null named')]
           in top
                <> [Element.el "input" ["search"] [Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter scenarios"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "aria-label" (Text.pack "Filter scenarios"), Element.on "input" MkSearch] []]
                <> featured
                <> [Element.el "h2" [] [] [Element.string "Every scenario"] | not (null featured)]
                <> [ Element.el "p" ["count"] [] [Element.string (show (length matching) <> " of " <> show (length names) <> (if length matching > shown then ", the first " <> show shown <> " shown" else ""))],
                     links (take shown matching)
                   ]

-- | How a game played on from a position ended (Parun.Browser.Replay.playOn),
-- with buttons back to that position and to play on from it again.
ended :: [Text.Text] -> Html.Html Event
ended said =
  Element.el
    "main"
    ["message", "branch-ended"]
    []
    ( [Element.el "p" [] [] [Element.text line] | line <- said]
        <> [ Element.el
               "p"
               ["ending-actions"]
               []
               [ Element.button [] MkResumeReplay [Element.string "Back to the replay"],
                 Element.button ["primary"] MkBranch [Element.string "Play on again"]
               ]
           ]
    )
