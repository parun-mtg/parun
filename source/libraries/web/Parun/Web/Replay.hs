-- | A scenario replayed on the page (#8821): the table at one position of
-- its timeline as a chosen seat sees it, a scrubber over the positions, the
-- entry the run takes next and the question it answers, the lists of
-- games finished here (Parun.Web.History) and of scenarios to open, and how
-- a game played on from a position ended.
module Parun.Web.Replay where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.Phase as Codec.Phase
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Move as Move
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.When as When
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.History as History
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
  | -- | Shows this list.
    MkShow Shelf
  | -- | Opens this game finished here, by its key (History.key).
    MkWatch Text.Text
  | -- | The lobby, on this game's decks.
    MkPlayAgain Text.Text
  | -- | Asks before deleting.
    MkForget Forgetting
  | MkConfirmForget
  | MkCancelForget
  deriving (Eq, Show)

-- | What a replay replays.
data Source
  = -- | A scenario, by its name beneath data/scenarios, less its extension.
    MkScenario Text.Text
  | -- | A game finished here, by its key (History.key).
    MkPlayed Text.Text
  deriving (Eq, Show)

-- | Which list the replays screen shows.
data Shelf
  = MkYourGames
  | MkScenarios
  deriving (Bounded, Enum, Eq, Show)

-- | What deleting deletes.
data Forgetting
  = -- | The game of this key.
    MkForgetGame Text.Text
  | MkForgetAll
  deriving (Eq, Show)

-- | The replays screen: the list shown, the games finished here, newest
-- first, the deletion asked about, and the filter typed over the scenarios.
data Listing = MkListing
  { shelf :: Shelf,
    games :: [History.Game],
    forgetting :: Maybe Forgetting,
    typed :: Text.Text
  }
  deriving (Eq, Show)

-- | The replays screen over these games, newest first: theirs shown first,
-- if any.
listing :: [History.Game] -> Listing
listing played = MkListing (if null played then MkScenarios else MkYourGames) (History.newestFirst played) Nothing Text.empty

-- | The replays screen after a tap that changes only what it shows.
listed :: Event -> Text.Text -> Listing -> Listing
listed event value shown = case event of
  MkShow chosen -> shown {shelf = chosen, forgetting = Nothing, typed = Text.empty}
  MkSearch -> shown {typed = value}
  MkForget what -> shown {forgetting = Just what}
  MkCancelForget -> shown {forgetting = Nothing}
  _ -> shown

-- | The replays screen once the deletion asked about is done.
forgot :: Listing -> Listing
forgot shown = case forgetting shown of
  Just (MkForgetGame k) -> shown {games = filter ((/= k) . History.key) (games shown), forgetting = Nothing}
  Just MkForgetAll -> shown {games = [], forgetting = Nothing}
  Nothing -> shown

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
    -- | That entry in a line (lineOf), where it answers or concedes.
    entryLine :: Maybe Text.Text,
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
            Element.button ["back"] MkBack [Element.string "All replays"]
          ],
        Element.el
          "div"
          ["segmented", "seats"]
          [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "View as")]
          [ Element.el "button" ["on" | pid == viewer frame] [Element.attr "type" (Text.pack "button"), Element.attr "title" (label <> Text.pack ": " <> Wording.player pid <> Text.pack " on the table"), Element.attr "aria-pressed" (Text.pack (if pid == viewer frame then "true" else "false")), Element.on "click" (MkSeat pid)] [Element.text label]
          | (pid, label) <- seats frame
          ]
      ]

-- | What the run does at the position: the entry it takes, the question it
-- answers there (its form, for the seat asked), or why it stopped.
stop :: Frame -> Maybe Request.Request -> [Html.Html Event]
stop frame asked =
  [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text problem] | problem <- Maybe.maybeToList (failure frame)]
    <> [Element.el "p" ["entry-line"] [] [Element.text l] | l <- Maybe.maybeToList (entryLine frame)]
    <> [ if Maybe.isJust (entryLine frame) then Element.el "details" ["entry-details"] [] [Element.el "summary" [] [] [Element.string "Details"], raw] else raw
       | e <- Maybe.maybeToList (entry frame),
         let raw = Element.el "pre" ["entry"] [] [Element.text e]
       ]
    <> case (question frame, asked) of
      (_, Just r) -> fmap (fmap MkTable) (Web.Question.form r Set.empty)
      (Just r, Nothing) -> [Element.el "p" ["waiting"] [] [Element.text (Text.pack "Waiting on " <> seatName frame (Question.seat (Request.question r)) <> Text.pack ": " <> Question.kind (Request.question r) <> Text.pack ".")]]
      (Nothing, Nothing) -> []

seatName :: Frame -> PlayerId.PlayerId -> Text.Text
seatName frame pid = maybe (Wording.player pid) snd (List.find ((== pid) . fst) (seats frame))

-- | A timeline entry in a line, each seat named as `names` names it by its
-- label, else by the label: "Turn 1 · Untap · You: shuffle". Only an answer
-- or a concession has one, the entries a played game records
-- (Parun.Play.Record.entryFor).
lineOf :: Map.Map Text.Text Text.Text -> Timed.Timed -> Maybe Text.Text
lineOf names timed =
  let at = Timed.when timed
      label = Label.unwrap (When.player at)
      what = case Timed.entry timed of
        Entry.Do (Move.Answer answer) -> Just (spaced (Answer.prompt answer))
        Entry.Do Move.Concede -> Just (Text.pack "concede")
        _ -> Nothing
   in fmap (\did -> Text.intercalate (Text.pack " \x00b7 ") [Text.pack ("Turn " <> show (When.turn at)), capitalised (spaced (Text.pack (Codec.Phase.flatName (When.phase at)))), Map.findWithDefault label label names <> Text.pack ": " <> did]) what
  where
    -- "ChooseAction" as "choose action".
    spaced = Text.toLower . Text.strip . Text.concatMap (\c -> if Char.isUpper c then Text.pack [' ', c] else Text.singleton c)
    capitalised t = maybe t (\(c, rest) -> Text.cons (Char.toUpper c) rest) (Text.uncons t)

-- | The sheet of the card being read.
sheet :: Frame -> Maybe (Html.Html Event)
sheet frame = do
  b <- board frame
  r <- reading frame
  o <- Wording.recalled b (Card.object r)
  pure (Card.sheet b o r [] (MkTable Web.Question.MkClose))

-- | The replays screen: a switch between the games finished here and the
-- scenarios, then the list shown.
picker :: Listing -> Maybe (Map.Map Text.Text [Text.Text], [Text.Text]) -> Html.Html Event
picker shown listedScenarios =
  Element.el "main" ["message", "scenarios"] [] $
    [ Element.el "h1" [] [] [Element.string "Replays"],
      Element.el
        "div"
        ["segmented", "shelves"]
        [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Replays to list")]
        [ Element.el "button" ["on" | on] [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if on then "true" else "false")), Element.on "click" (MkShow which)] [Element.string label]
        | (which, label) <- [(MkYourGames, "Your games"), (MkScenarios, "Scenarios")],
          let on = which == shelf shown
        ]
    ]
      <> case shelf shown of
        MkYourGames -> yourGames shown
        MkScenarios -> scenarioList (typed shown) listedScenarios

-- | The games finished here, newest first, each to watch, play again or
-- delete, and the way to delete them all, each deletion asked about first.
yourGames :: Listing -> [Html.Html Event]
yourGames shown = case games shown of
  [] -> [Element.el "p" ["muted"] [] [Element.string "The games you finish in this browser show here, to watch again."]]
  played ->
    [ Element.el "p" ["count"] [] [Element.string ("Kept in this browser: the last " <> show History.cap <> ".")],
      Element.el
        "section"
        ["panel", "your-games"]
        []
        [ Element.el "ul" ["game-rows"] [] (fmap gameRow played),
          case forgetting shown of
            Just MkForgetAll -> asking "Delete every game kept here? This cannot be undone." "Delete all"
            _ -> Element.el "div" ["history-actions"] [] [Element.button ["danger"] (MkForget MkForgetAll) [Element.string "Clear history"]]
        ]
    ]
  where
    gameRow game =
      let k = History.key game
       in case forgetting shown of
            Just (MkForgetGame asked) | asked == k -> History.row game [asking "Delete this game? This cannot be undone." "Delete"]
            _ ->
              History.row
                game
                ( [Element.button [] (MkWatch k) [Element.string "Watch"]]
                    <> [Element.button [] (MkPlayAgain k) [Element.string "Play again"] | all (Maybe.isJust . History.deckName) (History.seats game)]
                    <> [Element.button ["danger"] (MkForget (MkForgetGame k)) [Element.string "Delete"]]
                )
    asking asked confirm =
      Element.el
        "div"
        ["confirm-delete"]
        [Element.attr "role" (Text.pack "alertdialog"), Element.attr "aria-label" (Text.pack asked)]
        [ Element.el "p" [] [] [Element.string asked],
          Element.el "div" ["choices"] [] [Element.button ["danger"] MkConfirmForget [Element.string confirm], Element.button [] MkCancelForget [Element.string "Cancel"]]
        ]

-- | The list of scenarios once fetched: a filter, then the names matching
-- it, those the manifest names first under their themes
-- (Parun.Scenario.Load.parseManifest), then every one, each a button that
-- opens it, the first `shown` of them.
scenarioList :: Text.Text -> Maybe (Map.Map Text.Text [Text.Text], [Text.Text]) -> [Html.Html Event]
scenarioList typed' listedScenarios =
  let matches = Text.isInfixOf (Text.toLower typed') . Text.toLower
      shown = 200 :: Int
      links = Element.keyed "ul" ["scenario-list"] [] . fmap (\n -> (n, Element.el "li" [] [] [Element.button [] (MkOpen n) [Element.text n]]))
   in case listedScenarios of
        Nothing -> [Element.el "p" [] [] [Element.string "Fetching the scenarios\x2026"]]
        Just (themes, names) ->
          let matching = filter matches names
              featured = [Element.el "section" ["theme"] [] [Element.el "h2" [] [] [Element.text theme], links named'] | (theme, named) <- Map.toList themes, let named' = filter matches named, not (null named')]
           in [Element.el "input" ["search"] [Element.attr "type" (Text.pack "search"), Element.attr "placeholder" (Text.pack "Filter scenarios"), Element.attr "autocomplete" (Text.pack "off"), Element.attr "aria-label" (Text.pack "Filter scenarios"), Element.on "input" MkSearch] []]
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
