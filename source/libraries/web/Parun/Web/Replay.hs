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
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.Answer as Answer
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Entry as Entry
import qualified Parun.Types.Label as Label
import qualified Parun.Types.Move as Move
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as Reply
import qualified Parun.Types.Request as Request
import qualified Parun.Types.Timed as Timed
import qualified Parun.Types.When as When
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Board as Board
import qualified Parun.Web.BotSettings as BotSettings
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
  = -- | Steps this way from the position shown when the engine reads it, so
    -- taps queued while it works add up (`stepTo`).
    MkStep Step
  | -- | Goes to the position the scrubber's value names.
    MkScrub
  | -- | Lets the replay step on by itself (True), or pauses it.
    MkPlay Bool
  | -- | Shows the table as this seat sees it.
    MkSeat PlayerId.PlayerId
  | -- | A tap on the table or the question's form.
    MkTable Web.Question.Event
  | -- | Filters the list of scenarios by the text typed.
    MkSearch
  | -- | Opens this scenario.
    MkOpen Text.Text
  | -- | Plays on from the position shown, each seat filled so.
    MkBranch (Map.Map PlayerId.PlayerId Filling)
  | -- | Fills this seat as the value chosen names (fillingNamed).
    MkFill PlayerId.PlayerId
  | -- | Sets the difficulty of the bot filling this seat to the one named.
    MkLevel PlayerId.PlayerId
  | -- | Sets the persona of the bot filling this seat to the one named.
    MkStyle PlayerId.PlayerId
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
  | -- | Saves this game's scenario file to the player's device, by its key.
    MkDownload Text.Text
  | -- | Asks before deleting.
    MkForget Forgetting
  | MkConfirmForget
  | MkCancelForget
  deriving (Eq, Show)

-- | How a seat is filled when the replay is played on from a position.
--
-- Not implemented: a seat filled by a player invited from another browser
-- (#9777).
data Filling
  = -- | By the person at this screen.
    MkHere
  | -- | By the search bot at this difficulty and persona (Parun.Play.Opponent).
    MkBot Difficulty.Difficulty Persona.Persona
  | -- | By a goldfish that only passes (Parun.Play.Game.goldfish).
    MkGoldfish
  deriving (Eq, Show)

-- | Each kind of filling as the seat's menu values and shows it, a bot as
-- the lobby's default one ('BotSettings.defaultBot').
fillings :: [(Filling, (String, String))]
fillings =
  [ (MkHere, ("here", "This screen")),
    (uncurry MkBot BotSettings.defaultBot, ("bot", "Bot")),
    (MkGoldfish, ("goldfish", "Goldfish (only passes)"))
  ]

-- | The kind of filling the seat's menu value names.
fillingNamed :: Text.Text -> Maybe Filling
fillingNamed value = fst <$> List.find ((== Text.unpack value) . fst . snd) fillings

-- | Whether two fillings are of one kind, whatever a bot's settings.
sameKind :: Filling -> Filling -> Bool
sameKind one two = case (one, two) of
  (MkBot {}, MkBot {}) -> True
  _ -> one == two

-- | How a seat is filled to play on: as chosen, else by the person at this
-- screen if it is the seat viewed as, else by the lobby's default bot.
fillingOf :: Frame -> PlayerId.PlayerId -> Filling
fillingOf frame pid = Map.findWithDefault (if pid == viewer frame then MkHere else uncurry MkBot BotSettings.defaultBot) pid (chosenFillings frame)

-- | How every seat is filled to play on ('fillingOf').
plan :: Frame -> Map.Map PlayerId.PlayerId Filling
plan frame = Map.fromList [(pid, fillingOf frame pid) | (pid, _) <- seats frame]

-- | The fillings chosen once a seat's filling, or its bot's difficulty or
-- persona, is chosen as `value` names; any other tap changes none.
refilled :: Event -> Text.Text -> Frame -> Map.Map PlayerId.PlayerId Filling
refilled event value frame =
  let chosen = chosenFillings frame
      set pid f = Map.insert pid f chosen
      named nameOf = Opponent.named nameOf (Text.unpack value)
   in case event of
        MkFill pid -> maybe chosen (\f -> if sameKind f (fillingOf frame pid) then chosen else set pid f) (fillingNamed value)
        MkLevel pid | MkBot _ style <- fillingOf frame pid -> maybe chosen (\level -> set pid (MkBot level style)) (named Opponent.difficultyName)
        MkStyle pid | MkBot level _ <- fillingOf frame pid -> maybe chosen (set pid . MkBot level) (named Opponent.personaName)
        _ -> chosen

-- | Where a step button goes from a position.
data Step
  = MkToStart
  | -- | To the start of the turn the position is in, or of the one before at its start.
    MkTurnBack
  | MkStepBack
  | MkStepOn
  | -- | To the start of the next turn, or the end when no turn begins after.
    MkTurnOn
  | MkToEnd
  deriving (Bounded, Enum, Eq, Show)

-- | The position a step goes to from `at`, of `count` positions, turns
-- beginning at `starts`.
stepTo :: [Natural] -> Natural -> Natural -> Step -> Natural
stepTo starts count at way = case way of
  MkToStart -> 0
  MkTurnBack -> Maybe.fromMaybe 0 (Maybe.listToMaybe (reverse (filter (< at) starts)))
  MkStepBack -> Natural.minusSaturating at 1
  MkStepOn -> min count (at + 1)
  MkTurnOn -> Maybe.fromMaybe count (List.find (> at) starts)
  MkToEnd -> count

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
    -- | Each seat, and its name (seatsNamed).
    seats :: [(PlayerId.PlayerId, Text.Text)],
    viewer :: PlayerId.PlayerId,
    -- | How many entries the run has taken: from 0 to `positions`.
    position :: Natural,
    -- | How many entries the timeline holds.
    positions :: Natural,
    -- | The positions at which a turn begins (turnStarts), ascending.
    turns :: [Natural],
    -- | Whether the replay steps on by itself: page.js presses the next
    -- step while the page is marked so (`view`).
    playing :: Bool,
    -- | The table as the viewer sees it; Nothing where the run failed first.
    board :: Maybe PlayerView.PlayerView,
    -- | The table as the viewer saw it at each earlier position a turn
    -- begins, oldest first: what the log holds before the board's turn
    -- (logOf).
    earlier :: [PlayerView.PlayerView],
    -- | How the game ended, shown only at the end, so it does not spoil the
    -- replay; empty for a scenario.
    outcome :: Text.Text,
    -- | CR 730.1a: how many games the board's game is nested inside, as
    -- Request.depth counts them.
    depth :: Natural,
    -- | The entry the run takes next in a line (lineOf); Nothing at the end.
    entryLine :: Maybe Text.Text,
    -- | The question the entry answers or is checked at, as its seat is sent it.
    question :: Maybe Request.Request,
    -- | Why the run stopped short of this position.
    failure :: Maybe Text.Text,
    reading :: Maybe Card.Reading,
    -- | The Oracle text of each object the seat sees (Card.written).
    written :: Map.Map ObjectId.ObjectId [Text.Text],
    -- | The key of the game finished here replayed (History.key), to
    -- download; Nothing for a scenario.
    kept :: Maybe Text.Text,
    -- | How each seat was chosen to be filled to play on; 'fillingOf' a
    -- seat absent.
    chosenFillings :: Map.Map PlayerId.PlayerId Filling
  }

-- | The page for a frame: the table, and beside it the replay's controls.
-- The table only opens sheets: the question recorded at the position is
-- shown, not answered (stop).
view :: Frame -> Html.Html Event
view frame =
  let asked = question frame >>= \r -> if Question.seat (Request.question r) == viewer frame then Just r {Request.view = board frame} else Nothing
      logged = logOf (earlier frame <> Maybe.maybeToList (board frame))
   in Element.el "main" (["game", "replay"] <> ["playing" | playing frame && position frame < positions frame]) [] $
        foldMap (\b -> [Board.turnBar [] (Web.Question.subgameChip (depth frame) <> [Bar.settingsButton Icon.MkMenu "Menu" Nothing Nothing]) b]) (board frame)
          <> [Element.el "div" ["play-area"] [] (foldMap (\b -> [Board.view readOnly (written frame) b]) (board frame))]
          <> foldMap (Board.hand readOnly) (board frame)
          <> Web.Question.side (Text.pack "Replay") (controls frame <> stop frame asked) (reading frame) (sheet frame) (fmap (fmap MkTable) (Web.Question.logView (board frame) (viewer frame) Web.Question.blank {Web.Question.logged = logged}))

-- | The log through the last of these views, oldest first, as the live
-- game's keeps it (Parun.Browser.Hotseat): each turn's entries under its
-- heading, the turn before each view read from its last turn's.
logOf :: [PlayerView.PlayerView] -> Seq.Seq Log.Entry
logOf views = snd (List.foldl' (\(seen, logged) v -> let (moved, unread) = Log.unseen seen v in (moved, Log.appended logged unread)) (Map.empty, Seq.empty) views)

-- | Every tile opens its sheet, and no seat may be tapped.
readOnly :: Board.Marks Event
readOnly = Board.MkMarks (\o -> Card.MkCue [] (MkTable (Web.Question.MkRead (ObjectView.object o))) Nothing) (const Nothing)

-- | A scenario's title, and what of its description is left beneath it:
-- a leading rules citation ("CR 702.9b: flying blocks" as ("Flying
-- blocks", "CR 702.9b")), less the suite's "gameplay" tag; with no
-- description, its file name in words.
titled :: Text.Text -> Text.Text -> (Text.Text, Text.Text)
titled file said = case Text.breakOn (Text.pack ": ") said of
  (rules, rest)
    | Text.isPrefixOf (Text.pack "CR ") rules,
      let after = Text.strip (Text.drop 2 rest),
      not (Text.null after) ->
        (capitalised after, Maybe.fromMaybe rules (Text.stripSuffix (Text.pack " gameplay") rules))
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
-- icon over a word saying how far it goes, its tooltip where to, and the
-- way to let it play itself, or pause it; the ways to play on, to download
-- a game finished here and to leave; and the seats to view as, each by its name and, where the table
-- names it otherwise, that name.
controls :: Frame -> [Html.Html Event]
controls frame =
  let at = position frame
      count = positions frame
      starts = Set.fromList (turns frame)
      turnBack = stepTo (turns frame) count at MkTurnBack
      later = List.find (> at) (turns frame)
      -- Punctuation keys, since page.js takes L for the log.
      step label caption icon back hint key way enabled =
        Element.el
          "button"
          []
          ( [ Element.attr "type" (Text.pack "button"),
              Element.attr "aria-label" (Text.pack label),
              Element.attr "aria-keyshortcuts" (Text.pack key),
              Element.attr "title" (Text.pack (hint <> " (" <> key <> ")")),
              Element.on "click" (MkStep way)
            ]
              <> [Element.attr "disabled" Text.empty | not enabled]
          )
          [Icon.icon ["back" | back] icon, Element.el "span" [] [] [Element.string caption]]
   in [ Element.el
          "div"
          ["replay-head"]
          []
          ( [ Element.el "h2" ["replay-title"] [] (Mana.rich (name frame)),
              Element.el "p" ["replay-description"] [] (Mana.rich (description frame))
            ]
              -- Hidden until the end, so it does not spoil the replay.
              <> [Element.el "p" ["replay-outcome"] [] [Element.text (outcome frame)] | at >= count, not (Text.null (outcome frame))]
          ),
        Element.el
          "div"
          ["scrubber"]
          []
          [ Element.el "p" ["position"] [] [Element.string ("Step " <> show at <> " of " <> show count)],
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
              [ step "First position" "Start" Icon.MkSkip True "To the start of the replay" "<" MkToStart (at > 0),
                step "Previous turn" "Turn" Icon.MkTurn True (if turnBack == 0 then "To the start of the first turn" else if Set.member at starts then "To the start of the turn before" else "To the start of this turn") "[" MkTurnBack (at > 0),
                step "Previous position" "Step" Icon.MkStep True "Back one step" "," MkStepBack (at > 0),
                step "Next position" "Step" Icon.MkStep False "On one step" "." MkStepOn (at < count),
                step "Next turn" "Turn" Icon.MkTurn False (if Maybe.isJust later then "To the start of the next turn" else "To the end: no turn begins after this one") "]" MkTurnOn (at < count),
                step "Last position" "End" Icon.MkSkip False "To the end of the replay" ">" MkToEnd (at < count)
              ],
            let on = playing frame && at < count
             in Element.el
                  "button"
                  ["play"]
                  ( [ Element.attr "type" (Text.pack "button"),
                      Element.attr "aria-label" (Text.pack (if on then "Pause" else "Play")),
                      Element.attr "aria-keyshortcuts" (Text.pack "P"),
                      Element.attr "title" (Text.pack (if on then "Stop playing (P)" else "Step on by itself, a step a second (P)")),
                      Element.on "click" (MkPlay (not on))
                    ]
                      <> [Element.attr "disabled" Text.empty | not on, at >= count]
                  )
                  [Icon.icon [] (if on then Icon.MkPause else Icon.MkStep), Element.el "span" [] [] [Element.string (if on then "Pause" else "Play")]]
          ],
        Element.el
          "div"
          ["replay-actions"]
          []
          ( [ let anyHere = MkHere `elem` plan frame
               in Element.el
                    "button"
                    ["branch"]
                    ( [Element.attr "type" (Text.pack "button"), Element.on "click" (MkBranch (plan frame))]
                        <> [Element.attr "disabled" Text.empty | not anyHere]
                        <> [Element.attr "title" (Text.pack "Fill at least one seat at this screen") | not anyHere]
                    )
                    [Element.string "Play on from here"]
            ]
              <> [Element.button ["download"] (MkDownload k) [Element.string "Download"] | k <- Maybe.maybeToList (kept frame)]
              <> [Element.button ["back"] MkBack [Element.string "All replays"]]
          ),
        fillingsView frame,
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

-- | How each seat is filled to play on: a menu of the kinds of filling
-- under the seat's name, and a bot's difficulty and persona beneath it.
fillingsView :: Frame -> Html.Html Event
fillingsView frame =
  Element.el "fieldset" ["play-on-seats"] [] $
    Element.el "legend" ["subhead"] [] [Element.string "Play on with"]
      : concat
        [ Element.el
            "div"
            ["play-on-seat"]
            []
            [ Element.el "label" [] [Element.attr "for" (Text.pack field)] [Element.text label],
              Element.el
                "select"
                ["play-on-filling", "deck-menu"]
                [Element.attr "id" (Text.pack field), Element.on "change" (MkFill pid)]
                [ Element.el "option" [] ([Element.attr "value" (Text.pack value)] <> [Element.attr "selected" Text.empty | sameKind kind filling]) [Element.string shown]
                | (kind, (value, shown)) <- fillings
                ]
            ]
            : [BotSettings.botView ("seat-" <> show seat) (MkLevel pid) (MkStyle pid) (level, style) | MkBot level style <- [filling]]
        | (pid, label) <- seats frame,
          let seat = PlayerId.unwrap pid + 1
              field = "play-on-" <> show seat
              filling = fillingOf frame pid
        ]

-- | What the run does at the position: the entry it takes, in a line, then
-- the question it answers there (drawn dead for the seat asked, since a
-- replay only watches), or why it stopped.
stop :: Frame -> Maybe Request.Request -> [Html.Html Event]
stop frame asked =
  [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Element.text problem] | problem <- Maybe.maybeToList (failure frame)]
    <> [Element.el "p" ["entry-line"] [] [Element.text l] | l <- Maybe.maybeToList (entryLine frame)]
    <> case (question frame, asked) of
      (_, Just r) ->
        [ Element.el
            "section"
            ["recorded"]
            [Element.attr "inert" Text.empty, Element.attr "aria-label" (Text.pack "The question as asked")]
            (Element.el "p" ["recorded-note"] [] [Element.string "The question as asked:"] : fmap Html.inert (Web.Question.form r (written frame) Set.empty Nothing))
        ]
      (Just r, Nothing) -> [Element.el "p" ["waiting"] [] [Element.text (Text.pack "Waiting on " <> seatName frame (Question.seat (Request.question r)) <> Text.pack ".")]]
      (Nothing, Nothing) -> []

-- | A seat's name as the table shows it to the seat viewed as: "You" for
-- that seat, else its number (Parun.Web.Board.name).
tableName :: Frame -> PlayerId.PlayerId -> Text.Text
tableName frame = nameTo (viewer frame)

-- | A seat's name as the table shows it to the seat `viewed`.
nameTo :: PlayerId.PlayerId -> PlayerId.PlayerId -> Text.Text
nameTo viewed pid = if pid == viewed then Text.pack "You" else Board.name pid

-- | Each seat by the name `names` gives its label, else by its number
-- (Parun.Web.Board.name), never by the label a scenario writes.
seatsNamed :: Map.Map Text.Text Text.Text -> [(PlayerId.PlayerId, Text.Text)] -> [(PlayerId.PlayerId, Text.Text)]
seatsNamed names labelled = [(pid, Map.findWithDefault (Board.name pid) label names) | (pid, label) <- labelled]

-- | Each seat's label, mapped to its name as the table shows it to the seat
-- `viewed` (nameTo): what a scenario's entry lines call it (lineOf).
tableNames :: PlayerId.PlayerId -> [(PlayerId.PlayerId, Text.Text)] -> Map.Map Text.Text Text.Text
tableNames viewed labelled = Map.fromList [(label, nameTo viewed pid) | (pid, label) <- labelled]

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
-- "Turn 2 · Declare attackers · alice declares attackers"; "Before the
-- first turn" for its moment where the game has not `begun` (CR 103.8).
lineOf :: Map.Map Text.Text Text.Text -> Bool -> Maybe Request.Request -> Timed.Timed -> Text.Text
lineOf names begun asked timed =
  let at = Timed.when timed
      label = Label.unwrap (When.player at)
      seat = Map.findWithDefault label label names
      -- The seat named "You" (History.seatNames) takes the verb as given.
      by phrase = seat <> Text.pack " " <> (if seat == Text.pack "You" then phrase else thirdPerson phrase)
      kind = fmap (Question.kind . Request.question) asked
      asKind = by (maybe (Text.pack "decide") Web.Question.verbOf kind)
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
            Nothing -> chanceOf (Answer.prompt answer)
            Just r -> by (Maybe.fromMaybe (Web.Question.verbOf (Answer.prompt answer)) (actionOf r (Answer.with answer)))
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
      moment = if begun then [Text.pack ("Turn " <> show (When.turn at)), Web.Question.capitalized (Text.pack (Codec.Phase.flatName (When.phase at)))] else [Text.pack "Before the first turn"]
   in Text.intercalate (Text.pack " \x00b7 ") (moment <> [did])

-- | What chance decides, by the random prompt's kind (Parun.Play.Chance):
-- "Shuffle" as "a library is shuffled".
chanceOf :: Text.Text -> Text.Text
chanceOf kind = Text.pack $ case Text.unpack kind of
  "Shuffle" -> "a library is shuffled"
  "RollDie" -> "a die is rolled"
  "FlipCoin" -> "a coin is flipped"
  "RandomFirstPlayer" -> "who goes first is chosen at random"
  _ -> "something is chosen at random"

-- | The action a ChooseAction answer takes, by its index into the offer:
-- "pass", "cast Shock".
actionOf :: Request.Request -> Reply.Reply -> Maybe Text.Text
actionOf r reply = do
  let q = Request.question r
  Monad.guard (Question.kind q == Text.pack "ChooseAction")
  i <- Protocol.naturalOf reply >>= Natural.toInt
  option <- Protocol.arrayOf (Question.offer q) >>= Maybe.listToMaybe . drop i
  if Web.Question.isPass option then pure (Text.pack "pass") else fmap (`Offer.actionPhrase` option) (Request.view r)

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
  pure (Card.sheet (written frame) b o r [] (MkTable Web.Question.MkClose))

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

-- | The games finished here, newest first, each to watch, play again,
-- download or delete, and the way to delete them all, each deletion asked about first.
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
                    <> [Element.button [] (MkDownload k) [Element.string "Download"]]
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
-- titled from its description where the titles hold it (`described`,
-- Parun.Scenario.Load.parseTitles), else from its name (titled), that
-- opens it, the first `shown` of them.
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
-- with buttons back to that position and to play on from it again, as
-- each seat filled as `filled` (MkBranch).
ended :: Map.Map PlayerId.PlayerId Filling -> [Text.Text] -> Html.Html Event
ended filled said =
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
                 Element.button ["primary"] (MkBranch filled) [Element.string "Play on again"]
               ]
           ]
    )
