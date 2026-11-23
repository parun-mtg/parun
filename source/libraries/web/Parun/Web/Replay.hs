-- | A scenario replayed on the page (#8821): the table at one position of
-- its timeline as a chosen seat sees it, a scrubber over the positions, the
-- entry the run takes next and the question it answers, the lists of
-- games finished here (Parun.Web.History) and of scenarios to open, and how
-- a game played on from a position ended.
module Parun.Web.Replay where

import qualified Control.Monad as Monad
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
import qualified Parun.Play.Offer as Offer
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Move as Move
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as Reply
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
  { -- | The replay's title, a scenario's from its description (titled).
    name :: Text.Text,
    description :: Text.Text,
    -- | Each seat, and the name the scenario gives it.
    seats :: [(PlayerId.PlayerId, Text.Text)],
    viewer :: PlayerId.PlayerId,
    -- | How many entries the run has taken: from 0 to `positions`.
    position :: Natural,
    -- | How many entries the timeline holds.
    positions :: Natural,
    -- | The positions at which a turn begins (turnStarts), ascending.
    turns :: [Natural],
    -- | The table as the viewer sees it; Nothing where the run failed first.
    board :: Maybe PlayerView.PlayerView,
    -- | CR 730.1a: how many games the board's game is nested inside, as
    -- Request.depth counts them.
    depth :: Natural,
    -- | The entry the run takes next, as the scenario writes it, and its
    -- moment; Nothing at the end.
    entry :: Maybe Text.Text,
    -- | That entry in a line (lineOf).
    entryLine :: Maybe Text.Text,
    -- | The question the entry answers or is checked at, as its seat is sent it.
    question :: Maybe Request.Request,
    -- | Why the run stopped short of this position.
    failure :: Maybe Text.Text,
    reading :: Maybe Card.Reading,
    -- | The Oracle text of each spell on the stack (Card.written).
    written :: Map.Map ObjectId.ObjectId [Text.Text]
  }

-- | The page for a frame: the table, and beside it the replay's controls.
-- The table only opens sheets: the question recorded at the position is
-- shown, not answered (stop).
view :: Frame -> Html.Html Event
view frame =
  let asked = question frame >>= \r -> if Question.seat (Request.question r) == viewer frame then Just r {Request.view = board frame} else Nothing
      logged = foldMap (Seq.fromList . snd . Log.unseen Map.empty) (board frame)
   in Element.el "main" ["game", "replay"] [] $
        foldMap (\b -> [Board.turnBar [] (Web.Question.subgameChip (depth frame) <> [Bar.settingsButton Icon.MkMenu "Menu" Nothing Nothing]) b]) (board frame)
          <> [Element.el "div" ["play-area"] [] (foldMap (\b -> [Board.view readOnly (written frame) b]) (board frame))]
          <> foldMap (Board.hand readOnly) (board frame)
          <> Web.Question.side (Text.pack "Replay") (controls frame <> stop frame asked) (reading frame) (sheet frame) (fmap (fmap MkTable) (Web.Question.logView (board frame) (viewer frame) Web.Question.blank {Web.Question.logged = logged}))

-- | Every tile opens its sheet, and no seat may be tapped.
readOnly :: Board.Marks Event
readOnly = Board.MkMarks (\o -> Card.MkCue [] (MkTable (Web.Question.MkRead (ObjectView.object o))) Nothing) (const Nothing)

-- | A scenario's title, and what of its description is left beneath it:
-- a leading rules citation ("CR 702.9b: flying blocks" as ("Flying
-- blocks", "CR 702.9b")); with no description, its file name in words.
titled :: Text.Text -> Text.Text -> (Text.Text, Text.Text)
titled file said = case Text.breakOn (Text.pack ": ") said of
  (rules, rest)
    | Text.isPrefixOf (Text.pack "CR ") rules,
      let after = Text.strip (Text.drop 2 rest),
      not (Text.null after) ->
        (capitalised after, rules)
  _
    | Text.null (Text.strip said) -> (worded file, Text.empty)
    | otherwise -> (said, Text.empty)

-- | A scenario's file name in words: "combat/goblin-piker-attacks" as
-- "Goblin piker attacks".
worded :: Text.Text -> Text.Text
worded file =
  let spaced' = Text.replace (Text.pack "-") (Text.pack " ") (snd (Text.breakOnEnd (Text.pack "/") file))
   in maybe (capitalised spaced') (Text.append (Text.pack "CR ")) (Text.stripPrefix (Text.pack "cr ") spaced')

capitalised :: Text.Text -> Text.Text
capitalised t = maybe t (\(c, rest) -> Text.cons (Char.toUpper c) rest) (Text.uncons t)

-- | The scenario; where the run stands, the scrubber and its steps, each an
-- icon over a word saying how far it goes, its tooltip where to; the ways
-- to play on and to leave; and the seats to view as, each by its name and,
-- where the table names it otherwise, that name.
controls :: Frame -> [Html.Html Event]
controls frame =
  let at = position frame
      count = positions frame
      starts = Set.fromList (turns frame)
      earlier = Maybe.fromMaybe 0 (Maybe.listToMaybe (reverse (filter (< at) (turns frame))))
      later = List.find (> at) (turns frame)
      -- Punctuation keys, since page.js takes L for the log.
      step label caption icon back hint key target enabled =
        Element.el
          "button"
          []
          ( [ Element.attr "type" (Text.pack "button"),
              Element.attr "aria-label" (Text.pack label),
              Element.attr "aria-keyshortcuts" (Text.pack key),
              Element.attr "title" (Text.pack (hint <> " (" <> key <> ")")),
              Element.on "click" (MkGo target)
            ]
              <> [Element.attr "disabled" Text.empty | not enabled]
          )
          [Icon.icon ["back" | back] icon, Element.el "span" [] [] [Element.string caption]]
   in [ Element.el
          "div"
          ["replay-head"]
          []
          [ Element.el "h2" ["replay-title"] [] (Mana.rich (name frame)),
            Element.el "p" ["replay-description"] [] (Mana.rich (description frame))
          ],
        Element.el
          "div"
          ["scrubber"]
          []
          [ Element.el "p" ["position"] [] [Element.string (if at < count then "Before entry " <> show (at + 1) <> " of " <> show count else "After all " <> show count <> " entries")],
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
            Element.el
              "div"
              ["step-buttons"]
              []
              [ step "First position" "Start" Icon.MkSkip True "To the start of the replay" "<" 0 (at > 0),
                step "Previous turn" "Turn" Icon.MkTurn True (if earlier == 0 then "To the start of the first turn" else if Set.member at starts then "To the start of the turn before" else "To the start of this turn") "[" earlier (at > 0),
                step "Previous position" "Step" Icon.MkStep True "Back one entry" "," (Natural.minusSaturating at 1) (at > 0),
                step "Next position" "Step" Icon.MkStep False "On one entry" "." (min count (at + 1)) (at < count),
                step "Next turn" "Turn" Icon.MkTurn False (if Maybe.isJust later then "To the start of the next turn" else "To the end: no turn begins after this one") "]" (Maybe.fromMaybe count later) (at < count),
                step "Last position" "End" Icon.MkSkip False "To the end of the replay" ">" count (at < count)
              ]
          ],
        Element.el
          "div"
          ["replay-actions"]
          []
          [ Element.button ["branch"] MkBranch [Element.string "Play on from here"],
            Element.button ["back"] MkBack [Element.string "All replays"]
          ],
        Element.el "p" ["subhead"] [] [Element.string "View as"],
        Element.el
          "div"
          ["segmented", "seats"]
          [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "View as")]
          [ Element.el "button" ["on" | pid == viewer frame] [Element.attr "type" (Text.pack "button"), Element.attr "aria-pressed" (Text.pack (if pid == viewer frame then "true" else "false")), Element.on "click" (MkSeat pid)] [Element.text (if label == table then label else label <> Text.pack " (" <> table <> Text.pack ")")]
          | (pid, label) <- seats frame,
            let table = tableName frame pid
          ]
      ]

-- | What the run does at the position: the entry it takes, in a line and
-- as written behind Details, then the question it answers there (drawn
-- dead for the seat asked, since a replay only watches), or why it stopped.
stop :: Frame -> Maybe Request.Request -> [Html.Html Event]
stop frame asked =
  [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text problem] | problem <- Maybe.maybeToList (failure frame)]
    <> [Element.el "p" ["entry-line"] [] [Element.text l] | l <- Maybe.maybeToList (entryLine frame)]
    <> [ Element.el "details" ["entry-details"] [] [Element.el "summary" [] [] [Element.string "Details"], Element.el "pre" ["entry"] [] [Element.text e]]
       | e <- Maybe.maybeToList (entry frame)
       ]
    <> case (question frame, asked) of
      (_, Just r) ->
        [ Element.el
            "section"
            ["recorded"]
            [Element.attr "inert" Text.empty, Element.attr "aria-label" (Text.pack "The question as asked")]
            (Element.el "p" ["recorded-note"] [] [Element.string "The question as asked:"] : fmap Html.inert (Web.Question.form r Set.empty))
        ]
      (Just r, Nothing) -> [Element.el "p" ["waiting"] [] [Element.text (Text.pack "Waiting on " <> seatName frame (Question.seat (Request.question r)) <> Text.pack ".")]]
      (Nothing, Nothing) -> []

-- | A seat's name as the table shows it to the seat viewed as: "You" for
-- that seat, else its number (Parun.Web.Board.name).
tableName :: Frame -> PlayerId.PlayerId -> Text.Text
tableName frame pid = if pid == viewer frame then Text.pack "You" else Board.name pid

seatName :: Frame -> PlayerId.PlayerId -> Text.Text
seatName frame pid = maybe (Wording.player pid) snd (List.find ((== pid) . fst) (seats frame))

-- | The positions at which a turn begins: the first entry's, and each
-- entry's whose turn differs from the entry's before it.
turnStarts :: [Timed.Timed] -> [Natural]
turnStarts timed =
  let turned = fmap (When.turn . Timed.when) timed
   in [i | (i, t, before) <- zip3 [0 ..] turned (Nothing : fmap Just turned), Just t /= before]

-- | A timeline entry in a line, its seat named as `names` names it by its
-- label, else by the label, saying what the entry does as the question
-- asked there (`asked`, Nothing for chance) words it:
-- "Turn 2 · Declare attackers · alice declares attackers".
lineOf :: Map.Map Text.Text Text.Text -> Maybe Request.Request -> Timed.Timed -> Text.Text
lineOf names asked timed =
  let at = Timed.when timed
      label = Label.unwrap (When.player at)
      seat = Map.findWithDefault label label names
      -- The seat named "You" (History.seatNames) takes the verb as given.
      by phrase = seat <> Text.pack " " <> (if seat == Text.pack "You" then phrase else thirdPerson phrase)
      kind = fmap (Question.kind . Request.question) asked
      asKind = by (maybe (Text.pack "decide") verbOf kind)
      did = case Timed.entry timed of
        Entry.Expect _ -> Text.pack "the scenario checks the game"
        -- CR 734.1.
        Entry.Refuse _ -> by (Text.pack "try a choice the game takes back")
        Entry.Do move -> case move of
          Move.Concede -> by (Text.pack "concede")
          Move.Pass -> by (Text.pack "pass")
          Move.Cast _ -> by (Text.pack "cast a spell")
          Move.PlayLand _ -> by (Text.pack "play a land")
          Move.Activate _ -> by (Text.pack "activate an ability")
          Move.Answer answer -> case asked of
            Nothing -> Text.pack "chance: " <> Wording.spaced (Answer.prompt answer)
            Just r -> by (Maybe.fromMaybe (verbOf (Answer.prompt answer)) (actionOf r (Answer.with answer)))
          Move.Attack _ -> asKind
          Move.Block _ -> asKind
          Move.AssignDamage _ -> asKind
          Move.ChooseDefender _ -> asKind
          Move.ChooseAttackTarget _ -> asKind
          Move.OrderTimestamps _ -> asKind
          Move.ChooseOptional _ -> asKind
          Move.ChooseToPay _ -> asKind
          Move.ChooseWordSwap _ -> asKind
          Move.ChooseCopyTarget _ -> asKind
          Move.ChooseTargets _ -> asKind
          Move.OrderTriggers _ -> asKind
          Move.Take _ -> asKind
   in Text.intercalate (Text.pack " \x00b7 ") [Text.pack ("Turn " <> show (When.turn at)), Web.Question.capitalized (Text.pack (Codec.Phase.flatName (When.phase at))), did]

-- | The action a ChooseAction answer takes, by its index into the offer:
-- "pass", "cast Shock".
actionOf :: Request.Request -> Reply.Reply -> Maybe Text.Text
actionOf r reply = do
  let q = Request.question r
  Monad.guard (Question.kind q == Text.pack "ChooseAction")
  i <- Protocol.naturalOf reply >>= Natural.toInt
  option <- Protocol.arrayOf (Question.offer q) >>= Maybe.listToMaybe . drop i
  if Web.Question.isPass option then pure (Text.pack "pass") else fmap (`Offer.actionPhrase` option) (Request.view r)

-- | A question's kind as what its seat does: "ChooseTargets" as
-- "choose targets".
verbOf :: Text.Text -> Text.Text
verbOf kind = case Text.unpack kind of
  "ChooseAction" -> Text.pack "take an action"
  "DeclareMulligan" -> Text.pack "decide whether to mulligan"
  "MulliganAction" -> Text.pack "use a card before a mulligan"
  "OpeningHandAction" -> Text.pack "use a card in the opening hand"
  "OfferedCast" -> Text.pack "decide whether to cast"
  "OfferedMiracleReveal" -> Text.pack "decide whether to reveal a miracle"
  "OfferedExtra" -> Text.pack "decide whether to take an offer"
  "Bottom" -> Text.pack "put cards on the bottom"
  _ -> Wording.spaced kind

-- | A phrase's leading verb in the third person: "cast Shock" as "casts Shock".
thirdPerson :: Text.Text -> Text.Text
thirdPerson phrase =
  let (verb, rest) = Text.breakOn (Text.pack " ") phrase
      ends suffix = Text.isSuffixOf (Text.pack suffix) verb
      conjugated
        | Text.null verb = verb
        | any ends ["s", "sh", "ch", "x", "z"] = verb <> Text.pack "es"
        | ends "y", not (any ends ["ay", "ey", "oy", "uy"]) = Text.dropEnd 1 verb <> Text.pack "ies"
        | otherwise = verb <> Text.pack "s"
   in conjugated <> rest

-- | The sheet of a card read.
sheet :: Frame -> Card.Reading -> Maybe (Html.Html Event)
sheet frame r = do
  b <- board frame
  o <- Wording.recalled b (Card.object r)
  pure (Card.sheet b o r [] (MkTable Web.Question.MkClose))

-- | The replays screen, laid out as the Decks screen is: its heading, the
-- games finished here and the scenarios as tabs, then the list shown.
picker :: Listing -> Maybe (Map.Map Text.Text [Text.Text], Map.Map Text.Text Text.Text, [Text.Text]) -> Html.Html Event
picker shown listedScenarios =
  Element.el "main" ["scenarios"] [] $
    [ Element.el "div" ["browser-head"] [] [Element.el "h1" [] [] [Element.string "Replays"]],
      Element.el
        "div"
        ["shelves"]
        [Element.attr "role" (Text.pack "tablist"), Element.attr "aria-label" (Text.pack "Replays to list")]
        [ Element.el
            "button"
            (["tab"] <> ["on" | on])
            [Element.attr "type" (Text.pack "button"), Element.attr "role" (Text.pack "tab"), Element.attr "aria-selected" (Text.pack (if on then "true" else "false")), Element.on "click" (MkShow which)]
            [Element.string label]
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

-- | The list of scenarios once fetched: a filter, then the scenarios whose
-- name or title holds it, those the manifest names first under their themes
-- (Parun.Scenario.Load.parseManifest), then every one, each a button
-- titled from its description where it was read (`described`), else from
-- its name (titled), that opens it, the first `shown` of them.
scenarioList :: Text.Text -> Maybe (Map.Map Text.Text [Text.Text], Map.Map Text.Text Text.Text, [Text.Text]) -> [Html.Html Event]
scenarioList typed' listedScenarios =
  let shown = 200 :: Int
   in case listedScenarios of
        Nothing -> [Element.el "p" [] [] [Element.string "Fetching the scenarios\x2026"]]
        Just (themes, described, names) ->
          let title n = fst (titled n (Map.findWithDefault Text.empty n described))
              matches n = any (Text.isInfixOf (Text.toLower typed') . Text.toLower) [n, title n]
              links = Element.keyed "ul" ["scenario-list"] [] . fmap (\n -> (n, Element.el "li" [] [] [Element.button [] (MkOpen n) [Element.text (title n)]]))
              matching = filter matches names
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
