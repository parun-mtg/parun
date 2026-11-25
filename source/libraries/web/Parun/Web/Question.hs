-- | A line protocol request as a page: the seat's board, what it is asked,
-- and an answer built from taps. Each kind of question the scenarios reach has
-- its own form (Parun.WebQuestionSpec's corpus case is the proof); any other
-- shows its offer and takes a reply typed as JSON.
module Parun.Web.Question where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.CombatDamageOption as Codec.CombatDamageOption
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.ManaUnit as Codec.ManaUnit
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Integer as Integer
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Play.Offer as Offer
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Wording as Wording
import qualified Parun.Types.CombatDamageOption as CombatDamageOption
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Looked as Looked
import qualified Parun.Types.Mana as Mana.Type
import qualified Parun.Types.ManaRetention as ManaRetention
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Zone as Zone
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Bar as Bar
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Icon as Icon
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Mana as Mana

-- | What a tap or a typed line does to the answer under way.
data Event
  = -- | Answers with this reply.
    MkAnswer R.Reply
  | -- | Picks this option of this group, or drops it if picked.
    MkToggle Natural Natural
  | -- | Answers with the options picked.
    MkSubmit
  | -- | Answers with the reply typed as JSON into the element.
    MkTyped
  | -- | The seat has the screen, after a hand-off.
    MkReady
  | -- | Pays the rest of the cost as auto chooses (Offer.autoSource).
    MkAuto
  | -- | Drops every pick.
    MkClear
  | -- | Shows a card's text (Wording.details).
    MkRead ObjectId.ObjectId
  | -- | Hides the card shown.
    MkClose
  | -- | Concedes the game (CR 104.3a): passes now, and the hotseat answers
    -- its next concession question yes.
    MkConcede
  | -- | Leaves the game for the home screen, to resume it later.
    MkHome
  | -- | Lets the page answer this much for the seat.
    MkLevel Level
  | -- | Previews a card's text under a resting pointer.
    MkPeek ObjectId.ObjectId
  | -- | Ends the preview.
    MkUnpeek
  | -- | Makes this blocker the one the next attacker tapped is for.
    MkFocus Natural
  | -- | Passes now, and the priorities after it until this stop (passesFor).
    MkSkip Until
  deriving (Eq, Show)

-- | Where an answer stands after an event.
data Outcome
  = -- | Still picking, these options so far, each a group's and its index.
    MkPicking (Set.Set (Natural, Natural))
  | -- | Answered.
    MkReplied R.Reply
  | -- | Not answered, and why.
    MkRefused Text.Text
  deriving (Eq, Show)

-- | A question answered by picking among groups of options: its groups, the
-- reply the picks make, each group's picks in its options' order, and the
-- label of the button that sends it.
data Picker = MkPicker
  { groups :: [Group],
    finish :: [[R.Reply]] -> Either Text.Text R.Reply,
    send :: Text.Text
  }

-- | Options to pick from, how many at least, how many at most, and whether
-- a pick replaces the group's last.
data Group = MkGroup
  { title :: Text.Text,
    options :: [(R.Reply, Text.Text)],
    least :: Natural,
    most :: Natural,
    exclusive :: Bool
  }

-- | How much the seat lets the page answer for it, its own setting: full
-- control asks everything but the indistinguishable; standard passes a
-- priority offering nothing but a pass and mana abilities, but for one
-- showing a block on the seat's side (blocksShown); auto passes those and
-- an empty stack outside the seat's main phases and pays mana automatically.
data Level
  = MkFull
  | MkStandard
  | MkAutoPass
  deriving (Bounded, Enum, Eq, Show)

-- | A level as the page names it, and as it is stored (page.js).
levelName :: Level -> (String, String)
levelName l = case l of
  MkFull -> ("full", "Full control")
  MkStandard -> ("standard", "Standard")
  MkAutoPass -> ("auto", "Auto")

-- | A level by its stored name.
levelNamed :: String -> Maybe Level
levelNamed name = List.find ((== name) . fst . levelName) [minBound .. maxBound]

-- | How far the seat asked the page to pass for it (MkSkip), its own choice
-- and binding no other player, so no shortcut is proposed (CR 733.2b's
-- second example, as one seat's setting).
data Until
  = -- | The end of this turn: passes while the stack is empty, and declares
    -- no attackers.
    MkTurnEnds Natural
  | -- | The seat's priority in this step of this turn.
    MkStep Natural Phase.Phase
  | -- | Something on the stack.
    MkStacked
  deriving (Eq, Show)

-- | The reply a seat's stop makes for it, and the stop it keeps. Short of a
-- stop the seat passes a priority with an empty stack; MkTurnEnds keeps
-- through a response and ends with its turn; any other ends where it is
-- reached, at something on the stack, or at any question but priority (an
-- attack's blockers, say), so the seat is never answered for a real decision.
passesFor :: Until -> Request.Request -> (Maybe R.Reply, Maybe Until)
passesFor stop request = case (stop, Request.view request) of
  (_, Nothing) -> (Nothing, Nothing)
  (MkTurnEnds turn, Just shown)
    | PlayerView.turn shown /= turn -> (Nothing, Nothing)
    | not (null (PlayerView.stack shown)) -> (Nothing, Just stop)
    | kind == "ChooseAction" -> (pass, Just stop)
    | kind == "DeclareAttackers" -> (Just (R.Array []), Just stop)
    | otherwise -> (Nothing, Just stop)
  (_, Just shown)
    | reached shown || not (null (PlayerView.stack shown)) || kind /= "ChooseAction" -> (Nothing, Nothing)
    | otherwise -> (pass, Just stop)
  where
    kind = Text.unpack (Question.kind (Request.question request))
    pass = R.Number <$> passIndex request
    reached shown = case stop of
      MkStep turn phase -> PlayerView.turn shown /= turn || PlayerView.phase shown >= phase
      _ -> False

-- | The reply that finishes a payment auto is making: auto's next source,
-- or paying once the cost is covered.
paying :: Request.Request -> Maybe R.Reply
paying request = case Text.unpack (Question.kind (Request.question request)) of
  "ChooseExtraManaSource" -> Just R.Null
  _ -> auto request

-- | A reply the seat is never asked for: the hotseat never concedes unasked
-- (CR 104.3a), the seat plays first when it chooses, a priority offering nothing but a pass and mana abilities is
-- passed (Offer.onlyPass) but where standard shows a block (blocksShown), and a choice between indistinguishable options is
-- made: identical mana to spend, and all of a creature's combat damage to
-- its one recipient (CR 510.1c).
automatic :: Level -> Request.Request -> Maybe R.Reply
automatic setting request = case Text.unpack (Question.kind question) of
  "Concede" -> Just . R.Text $ Text.pack "Continues"
  -- The player is taken to want to play first (CR 103.1), a default of this
  -- client's rather than the engine's, which full control asks.
  "PlayOrDraw" | setting /= MkFull -> Just . R.Text $ Text.pack "Play"
  "ChooseStarter" | setting /= MkFull -> Just . R.Text . Text.pack . show . PlayerId.unwrap $ Question.seat question
  "ChooseAction"
    | setting /= MkFull,
      Just i <- Offer.onlyPass (Question.offer question),
      not (setting == MkStandard && maybe False blocksShown (Request.view request)) ->
        Just (R.Number i)
    -- Auto passes an empty stack outside the seat's own main phases.
    | setting == MkAutoPass,
      Just shown <- Request.view request,
      null (PlayerView.stack shown),
      not (PlayerView.active shown == PlayerView.viewer shown && elem (PlayerView.phase shown) [Phase.PrecombatMain, Phase.PostcombatMain]) ->
        R.Number <$> passIndex request
  "ChooseManaToSpend" | Just (unit : rest) <- Protocol.arrayOf offer, all (== unit) rest -> Just unit
  "AssignCombatDamage"
    | Just [recipient] <- Protocol.field "lethal" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf,
      Just total <- Protocol.field "total" offer >>= Protocol.naturalOf ->
        Just $ R.Array [R.Array [fst recipient, R.Number $ toInteger total] | total > 0]
  _ -> Nothing
  where
    question = Request.question request
    offer = Question.offer question

-- | The picks a request's form starts with: combat damage divided lethal to
-- each recipient in turn and the rest to the last, a split a trampler's
-- excess allows (CR 510.1c, 702.19b), for the seat to change or confirm;
-- nothing picked for any other.
initial :: Request.Request -> Set.Set (Natural, Natural)
initial request = case (Text.unpack (Question.kind question), damageOf request) of
  ("AssignCombatDamage", Just (lethal, total)) ->
    let lastIndex = length lethal - 1
        go _ [] = []
        go remaining ((g, threshold) : rest) =
          let given = if g == lastIndex then remaining else min threshold remaining
           in (Int.toNaturalSaturating g, given) : go (Natural.minusSaturating remaining given) rest
     in Set.fromList (go total (zip [0 :: Int ..] (fmap snd lethal)))
  -- One to each target, the rest to the first (option n is n + 1).
  ("ChooseDivision", _)
    | Just targets <- Protocol.field "targets" offer >>= Protocol.arrayOf,
      Just total <- Monad.mplus (Protocol.field "total" offer >>= Protocol.naturalOf) (fmap sum (Protocol.field "parts" offer >>= Protocol.arrayOf >>= mapM Protocol.naturalOf)) ->
        Set.fromList [(g, if g == 0 then Natural.minusSaturating total (Natural.length targets) else 0) | g <- [0 .. Natural.length targets - 1]]
  -- Each player keeps their own life total.
  ("ChooseRedistribution", _) -> Set.fromList [(g, g) | g <- [0 .. maybe 0 Natural.length (Protocol.field "totals" offer >>= Protocol.arrayOf) - 1]]
  -- Every creature in the first pile.
  ("DividePiles", _) -> Set.fromList [(g, 0) | g <- [0 .. maybe 0 Natural.length (Protocol.field "from" offer >>= Protocol.arrayOf) - 1]]
  -- Each amount at its floor; a slot's number of targets at its most.
  (kind, _)
    | Just caps <- Protocol.field "amounts" offer ->
        let floors = foldMap Protocol.amountLeaves (Protocol.field "floors" offer)
         in Set.fromList [(g, if kind == "AnnounceTargets" then cap else Maybe.fromMaybe 0 (lookup path floors)) | (g, (path, cap)) <- zip [0 ..] (Protocol.amountLeaves caps)]
  _ -> Set.empty
  where
    question = Request.question request
    offer = Question.offer question

-- | A damage assignment's recipients with their lethal damage, and its total.
damageOf :: Request.Request -> Maybe ([(R.Reply, Natural)], Natural)
damageOf request = do
  let offer = Question.offer (Request.question request)
  lethal <- Protocol.field "lethal" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf
  total <- Protocol.field "total" offer >>= Protocol.naturalOf
  pure (lethal, total)

-- | Whether the seat is in the declare blockers step (CR 509) with a block
-- on its side of a combat: a creature of its blocking or blocked, which
-- standard stops at so the seat sees the blocks before damage.
blocksShown :: PlayerView.PlayerView -> Bool
blocksShown shown =
  PlayerView.phase shown == Phase.Combat CombatStep.DeclareBlockers
    && or [ours attacker || any ours blocking | (attacker, blocking) <- Map.toList (PlayerView.blockers shown)]
  where
    ours o = any (\v -> ObjectView.object v == o && ObjectView.controller v == Just (PlayerView.viewer shown)) (PlayerView.battlefield shown)

-- | Where a ChooseAction offer's pass sits.
passIndex :: Request.Request -> Maybe Integer
passIndex request = do
  Monad.guard $ Question.kind (Request.question request) == Text.pack "ChooseAction"
  options' <- Protocol.arrayOf (Question.offer (Request.question request))
  fmap toInteger (List.findIndex isPass options')

isManaAbility :: R.Reply -> Bool
isManaAbility option = (Protocol.field "action" option >>= Offer.textOf) == Just (Text.pack "ActivateManaAbility")

-- | Whether passing priority may give the offered action up.
lapses :: R.Reply -> Bool
lapses option = Protocol.field "lapses" option == Just (R.Boolean True)

isPass :: R.Reply -> Bool
isPass option = (Protocol.field "action" option >>= Offer.textOf) == Just (Text.pack "Pass")

-- | The source auto pays a payment's next part with (Offer.autoSource).
auto :: Request.Request -> Maybe R.Reply
auto request = do
  Monad.guard $ Question.kind (Request.question request) == Text.pack "ChooseManaSource"
  shown <- Request.view request
  fst <$> Offer.autoSource shown (Question.offer $ Request.question request)

-- | What the question screen shows besides the request: the options picked
-- so far, the card the seat is reading, and the game log.
data Screen = MkScreen
  { selected :: Set.Set (Natural, Natural),
    reading :: Maybe Card.Reading,
    -- | Everything the seat has seen happen, oldest first.
    logged :: Seq.Seq Log.Entry,
    -- | How many of the newest entries arrived since the seat last decided.
    fresh :: Natural,
    -- | The blocker an attacker's tile assigns to when tapped (MkFocus).
    focus :: Maybe Natural,
    level :: Level,
    -- | The Oracle text of each object the seat sees (Card.written).
    written :: Map.Map ObjectId.ObjectId [Text.Text],
    -- | Whether the screen names the seat it shows, on the turn bar and
    -- in the prompt: the person at it plays several seats, or it is handed
    -- between people (handsOver).
    naming :: Bool
  }
  deriving (Eq, Show)

-- | A screen with nothing picked, read or logged.
blank :: Screen
blank = MkScreen Set.empty Nothing Seq.empty 0 Nothing MkStandard Map.empty False

-- | The page for a request: the turn bar, the table with each tile marked
-- for what tapping it does, and beside it the question with its answers,
-- the card being read and the log.
view :: Request.Request -> Screen -> Html.Html Event
view request screen =
  table request screen (autoPass (level screen)) (barControls True (MkConcede <$ passIndex request)) (cues request (selected screen) (focus screen)) $
    foldMap (pure . eyebrow) (Request.view request)
      <> turnBegun (viewerOf request) screen
      <> [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] (Mana.rich problem) | problem <- Maybe.maybeToList (Request.problem request)]
      <> [Element.el "p" ["playing"] [Element.attr "role" (Text.pack "note")] [Html.text (Text.pack "You're playing " <> Wording.player (Question.seat (Request.question request)) <> Text.pack ".")] | naming screen]
      <> [Element.el "p" ["controlling"] [Element.attr "role" (Text.pack "note")] [Html.text (Text.pack "You're deciding for " <> Wording.player p <> Text.pack ", whom you control.")] | p <- Maybe.maybeToList (Question.for (Request.question request))]
      <> form request (written screen) (selected screen) (focus screen)

-- | The newest turn begun since the seat last decided, unless it never has:
-- passing for the seat can carry it into the next turn, which this says
-- before its next tap.
turnBegun :: PlayerId.PlayerId -> Screen -> [Html.Html msg]
turnBegun viewer screen =
  [ Element.el "p" ["nudge", "turned"] [Element.attr "role" (Text.pack "status")] [Element.text (Text.pack ("Turn " <> show n <> " began. It's ") <> (if active == viewer then Text.pack "your" else Wording.player active <> Text.pack "'s") <> Text.pack " turn.")]
  | total > newest,
    Log.MkTurn n active : _ <- [reverse (filter Log.isTurn (Foldable.toList (Seq.drop (total - newest) (logged screen))))]
  ]
  where
    total = Seq.length (logged screen)
    newest = Natural.toIntSaturating (fresh screen)

-- | Once blocks are declared, each attacker and the creatures blocking it,
-- or that it is unblocked (CR 509.1h), a line each.
combatPairs :: PlayerView.PlayerView -> [Html.Html msg]
combatPairs shown =
  [ nudge (Wording.choosable shown attacker <> maybe (Text.pack " is unblocked.") blockedBy (Map.lookup attacker (PlayerView.blockers shown)))
  | PlayerView.phase shown > Phase.Combat CombatStep.DeclareAttackers,
    PlayerView.phase shown <= Phase.Combat CombatStep.EndOfCombat,
    attacker <- Map.keys (PlayerView.attackers shown)
  ]
  where
    -- CR 509.1h: still blocked once every blocker is removed from combat.
    blockedBy bs
      | Set.null bs = Text.pack " is blocked."
      | otherwise = Text.pack " is blocked by " <> Wording.listing (fmap (lowered . Wording.choosable shown) (Set.toList bs)) <> Text.pack "."
    lowered t = maybe t (Text.pack "your " <>) (Text.stripPrefix (Text.pack "Your ") t)

-- | Above a prompt, whose turn it is and its step, in the turn's colour
-- while it is the seat's.
eyebrow :: PlayerView.PlayerView -> Html.Html msg
eyebrow board = Element.el "p" (["eyebrow", "moment"] <> ["yours" | Board.yours board]) [] [Element.text (Board.moment board)]

-- | The page while the game goes on without the seat: a request it was not
-- asked (Hotseat answers it), or the last it was while another browser's
-- seat decides; its board shown as it stands, less whose priority it is,
-- since the board is not redrawn as priority passes, and less what it
-- offered, so a card's sheet carries no action; in place of a question
-- what the game is waiting on, the seats of `deciding` if any; and
-- the auto-pass level, which Hotseat keeps for the seat's next request.
-- While those seats decide, the menu leaves for the home screen, and
-- concedes (CR 104.3a) unless the seat has, `conceded`, which it then does
-- at its next priority. Nothing unless the screen faces (the first) the
-- seat asked, since the board shows that seat's hidden cards.
watching :: Maybe PlayerId.PlayerId -> [PlayerId.PlayerId] -> Bool -> Request.Request -> Screen -> Maybe (Html.Html Event)
watching facing deciding conceded request screen = do
  Monad.guard $ facing == Just (Question.seat (Request.question request))
  let status = case (deciding, Request.view request) of
        (_ : _, _) -> waitingOn deciding <> (if conceded then " You concede when you next have priority." else "")
        (_, Just b)
          | not (PlayerView.begun b) -> "Waiting for the game to begin\x2026"
          | not (null (PlayerView.stack b)) -> "Resolving\x2026"
          | PlayerView.active b /= PlayerView.viewer b -> Text.unpack (Wording.player (PlayerView.active b)) <> " is taking their turn\x2026"
        _ -> "Moving on\x2026"
      shown = request {Request.view = fmap (\b -> b {PlayerView.priority = Nothing}) (Request.view request), Request.question = (Request.question request) {Question.offer = R.Null}}
  pure . table shown screen (autoPass (level screen)) (if null deciding then barControls False Nothing else barControls True (if conceded then Nothing else Just MkConcede)) (Board.MkMarks (\o -> Card.MkCue [] (MkRead (ObjectView.object o)) Nothing) (const Nothing)) $
    foldMap (pure . eyebrow) (Request.view request) <> [Element.el "p" ["waiting"] [] [Icon.loading ["small"], Element.string status]]

-- | What the game waits on while these seats decide, or the other
-- players if none is named.
waitingOn :: [PlayerId.PlayerId] -> String
waitingOn deciding = case deciding of
  [] -> "Waiting for the other players\x2026"
  _ -> "Waiting for " <> List.intercalate " or " (fmap (Text.unpack . Wording.player) deciding) <> " to decide\x2026"

-- | The seat the board shows, on the turn bar, where the screen names it.
seatChip :: Bool -> PlayerView.PlayerView -> [Html.Html msg]
seatChip named board =
  [ Element.el "span" ["seat-chip", "seat-" <> show (mod (PlayerId.unwrap (PlayerView.viewer board)) 6)] [Element.attr "title" (Text.pack "The seat this screen shows")] [Html.text (Wording.player (PlayerView.viewer board))]
  | named
  ]

-- | A subgame's mark on the turn bar (CR 730.1a), and how deep it is.
subgameChip :: Natural -> [Html.Html msg]
subgameChip depth =
  [ Element.el "span" ["subgame"] [Element.attr "title" (Text.pack "A subgame: the main game waits until it ends (CR 730)")] [Element.string (if depth == 1 then "Subgame" else "Subgame, " <> show depth <> " deep")]
  | depth > 0
  ]

-- | The auto-pass level, at the end of the turn bar's phases: chosen from
-- a disclosure.
autoPass :: Level -> Html.Html Event
autoPass current =
  Element.el
    "details"
    ["auto-pass"]
    []
    [ Element.el "summary" [] [Element.attr "title" (Text.pack "Automatic passing"), Element.attr "aria-label" (Text.pack ("Automatic passing: " <> snd (levelName current)))] [Element.el "span" ["k"] [] [Element.string "Stops: "], Element.string (snd (levelName current))],
      Element.el
        "div"
        ["segmented"]
        [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (Text.pack "Automatic passing")]
        [ pressable ["on" | l == current] [Element.attr "data-level" (Text.pack (fst (levelName l)))] (pressedIf (l == current)) (MkLevel l) [Element.string (snd (levelName l))]
        | l <- [minBound .. maxBound]
        ]
    ]

-- | The turn bar's controls: the menu button, whose menu (page.js) leaves
-- the game for the home screen where `asked`, and concedes (CR 104.3a) by
-- the event given where the seat may.
barControls :: Bool -> Maybe Event -> [Html.Html Event]
barControls asked concede = [Bar.settingsButton Icon.MkMenu "Menu" (if asked then Just MkHome else Nothing) concede]

-- | The links a log entry's names make: to the sheet of each object the
-- view shows.
linksOf :: Maybe PlayerView.PlayerView -> Maybe (Map.Map Text.Text ObjectId.ObjectId, ObjectId.ObjectId -> Event)
linksOf = fmap (\b -> (Log.linkable b, MkRead))

-- | The seat the request asks, as its view names it.
viewerOf :: Request.Request -> PlayerId.PlayerId
viewerOf request = maybe (Question.seat (Request.question request)) PlayerView.viewer (Request.view request)

-- | The game screen around a prompt's contents: the turn bar carrying the
-- auto-pass control and the controls given, the table, the hand, and the
-- side column (side).
table :: Request.Request -> Screen -> Html.Html Event -> [Html.Html Event] -> Board.Marks Event -> [Html.Html Event] -> Html.Html Event
table request screen passing controls marks prompt =
  let board = Request.view request
   in Element.el "main" ["game"] [] $
        foldMap (\b -> [Board.turnBar [passing] (seatChip (naming screen) b <> subgameChip (Request.depth request) <> controls) b]) board
          <> [Element.el "div" ["play-area"] [] (foldMap (\b -> [Board.view marks (written screen) b]) board)]
          <> foldMap (Board.hand marks) board
          <> side (Text.pack "Your decision") prompt (reading screen) (\r -> board >>= \b -> sheetFor request (written screen) b marks r) (logView board (viewerOf request) screen)

-- | The side column, top to bottom: the prompt, labelled; the card tapped,
-- which on a phone opens over the table instead (style.css); and the log.
-- Then, beside the column, the card previewed, which floats by the pointer
-- (page.js's placePeek) and leaves the column as it was.
side :: Text.Text -> [Html.Html msg] -> Maybe Card.Reading -> (Card.Reading -> Maybe (Html.Html msg)) -> [Html.Html msg] -> [Html.Html msg]
side label prompt opened sheetOf entries =
  let shown tapped = do r <- opened; Monad.guard (Card.pinned r == tapped); sheetOf r
      card = shown True
   in [ Element.el "aside" ["side"] [] $
          [ Element.el "section" ["prompt"] [Element.attr "aria-label" label] prompt,
            Element.el "section" (["inspector"] <> ["empty" | Maybe.isNothing card]) [Element.attr "aria-label" (Text.pack "Card")] [Maybe.fromMaybe (Element.el "p" ["inspector-hint"] [] [Element.string "Select a card to read it here."]) card]
          ]
            <> entries
      ]
        <> Maybe.maybeToList (shown False)

-- | The log: a heading saying how many entries are new and opening the
-- whole log, under its own heading, over the table below the turn bar (L),
-- then the newest few entries, hidden while the whole log is open
-- (style.css). Each turn is
-- under its own heading, newest first, the entries since the seat last
-- decided lit; nothing while the log is empty.
logView :: Maybe PlayerView.PlayerView -> PlayerId.PlayerId -> Screen -> [Html.Html Event]
logView board viewer screen =
  let entries = Seq.mapWithIndex (,) (logged screen)
      newest = Natural.toIntSaturating (fresh screen)
      total = Seq.length (logged screen)
      unread = Seq.length (Seq.filter (not . Log.isTurn) (Seq.drop (total - newest) (logged screen)))
      -- The newest entries link their cards; older ones stay plain text.
      listed limit = Log.grouped viewer (\j -> if j < 60 then linksOf board else Nothing) (>= total - newest) (Seq.take limit (Log.newestFirst entries))
   in [ Element.el
          "section"
          ["log-pane"]
          [Element.attr "aria-label" (Text.pack "Log")]
          [ Element.el
              "details"
              ["log"]
              []
              [ Element.el
                  "summary"
                  []
                  [Element.attr "title" (Text.pack "The whole log (L)"), Element.attr "aria-keyshortcuts" (Text.pack "L")]
                  ( [Element.el "span" ["eyebrow"] [] [Element.string "Log"]]
                      <> [Element.el "span" ["unread"] [] [Element.string (show unread <> " new")] | unread > 0]
                      <> [Element.el "span" ["expand"] [] [Element.el "span" ["closed"] [] [Element.string "Expand"], Element.el "span" ["opened"] [] [Element.string "Close"], Element.el "kbd" ["keys-only"] [Element.attr "aria-hidden" (Text.pack "true")] [Element.string "L"]]]
                  ),
                Element.el "div" ["log-panel"] [Element.attr "role" (Text.pack "region"), Element.attr "aria-labelledby" (Text.pack "log-panel-heading")] [Element.el "h2" ["log-panel-heading"] [Element.attr "id" (Text.pack "log-panel-heading")] [Element.string "The whole log"], listed 300]
              ],
            Element.el "div" ["log-recent"] [Element.attr "aria-live" (Text.pack "polite")] [listed 12]
          ]
      | total > 0
      ]

-- | The sheet for the card being read, with what the seat may do with it
-- now: its actions, or picking it.
sheetFor :: Request.Request -> Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> Board.Marks Event -> Card.Reading -> Maybe (Html.Html Event)
sheetFor request texts board marks r = do
  o <- Wording.recalled board (Card.object r)
  let cue = Board.object marks o
      actions = case Card.tap cue of
        MkRead _ -> [primary (MkAnswer (R.Number i)) label | (i, label) <- Map.findWithDefault [] (Card.object r) (actionsByObject texts request)]
        MkToggle g i -> [primary (MkToggle g i) (Text.pack (if elem "chosen" (Card.marks cue) then "Unselect" else "Select"))]
        MkAnswer reply -> [primary (MkAnswer reply) (Text.pack "Use")]
        MkFocus g -> [primary (MkFocus g) (Text.pack "Block with this")]
        _ -> []
  pure (Card.sheet texts board o r actions MkClose)

-- | Each object's actions in a ChooseAction offer, by index and label;
-- every copy of a card in hand its own, though the prompt lists one
-- (Offer.distinct).
actionsByObject :: Map.Map ObjectId.ObjectId [Text.Text] -> Request.Request -> Map.Map ObjectId.ObjectId [(Integer, Text.Text)]
actionsByObject texts request = case (Text.unpack (Question.kind (Request.question request)), Request.view request, Protocol.arrayOf (Question.offer (Request.question request))) of
  ("ChooseAction", Just shown, Just options') ->
    Map.fromListWith (flip (<>)) [(oid, [(i, actionLabel texts shown option)]) | (i, option) <- zip [0 ..] options', Just oid <- [Protocol.field "object" option >>= Offer.objectOf]]
  _ -> Map.empty

-- | An action as its button names it (Offer.actionName), an activation's
-- text as the sheet of its source words the ability (Card.asShown).
actionLabel :: Map.Map ObjectId.ObjectId [Text.Text] -> PlayerView.PlayerView -> R.Reply -> Text.Text
actionLabel texts shown option = Offer.actionName shown $ case (option, Protocol.field "object" option >>= Offer.objectOf) of
  (R.Object pairs, Just oid)
    | (Protocol.field "action" option >>= Offer.textOf) == Just (Text.pack "Activate") ->
        R.Object [(k, if k == Text.pack "text" then maybe v (R.Text . Card.asShown texts shown oid) (Offer.textOf v) else v) | (k, v) <- pairs]
  _ -> option

-- | How each tile and seat looks and what tapping it does for this request:
-- a card the seat may act with is lit and opens its sheet; a payment's
-- sources pay when tapped; an option of a picker toggles when tapped, where
-- it is offered once; every other tile opens its sheet.
cues :: Request.Request -> Set.Set (Natural, Natural) -> Maybe Natural -> Board.Marks Event
cues request chosen focused =
  let question = Request.question request
      offer = Question.offer question
      -- Lit: a card with an action besides a mana ability, which every
      -- untapped land has.
      acting = Set.fromList [oid | Question.kind question == Text.pack "ChooseAction", Just options' <- [Protocol.arrayOf offer], option <- options', not (isManaAbility option), Just oid <- [Protocol.field "object" option >>= Offer.objectOf]]
      sources = case Text.unpack (Question.kind question) of
        kind | kind `elem` ["ChooseManaSource", "ChooseExtraManaSource"] -> Map.fromList [(oid, source) | (source, _) <- foldMap (Maybe.mapMaybe Offer.manaSource) (Protocol.field "sources" offer >>= Protocol.arrayOf), Just oid <- [Offer.objectOf source]]
        _ -> Map.empty
      offered :: [(Natural, Natural, R.Reply)]
      offered = case picker request of
        Just chooser | Maybe.isNothing (ordering request), Maybe.isNothing (piles request) -> [(g, i, option) | (g, group) <- zip [0 ..] (groups chooser), (i, (option, _)) <- zip [0 ..] (options group)]
        _ -> []
      once f = Map.mapMaybe (\xs -> case xs of [x] -> Just x; _ -> Nothing) (Map.fromListWith (<>) [(k, [(g, i)]) | (g, i, option) <- offered, Just k <- [f option]])
      objects = once (\option -> Monad.mplus (Offer.objectOf option) (Offer.recipientObject option))
      players = once Offer.recipientPlayer
      toggled (g, i) = Card.MkCue [if Set.member (g, i) chosen then "chosen" else "choosable"] (MkToggle g i) Nothing
      -- Blocks are made on the table: a blocker's tile, then the tile of the
      -- attacker it blocks.
      (blockers, attackers) = Maybe.fromMaybe ([], []) (blocks request)
      blockerCue oid = do
        g <- List.elemIndex oid blockers
        let gn = Int.toNaturalSaturating g
            assigned = any ((== gn) . fst) (Set.toList chosen)
        pure (Card.MkCue [if focused == Just gn then "focused" else "choosable"] (MkFocus gn) Nothing `withMark` ["blocking" | assigned])
      attackerCue oid = do
        i <- Int.toNaturalSaturating <$> List.elemIndex oid attackers
        pure $ case focused of
          Just g -> Card.MkCue ["attacking", if Set.member (g, i) chosen then "chosen" else "choosable"] (MkToggle g i) Nothing
          Nothing -> Card.MkCue ["attacking"] (MkRead oid) Nothing
      objectCue o =
        let oid = ObjectView.object o
         in case (blockerCue oid, attackerCue oid, Map.lookup oid objects, Map.lookup oid sources) of
              (Just cue, _, _, _) -> cue
              (_, Just cue, _, _) -> cue
              (_, _, Just gi, _) -> toggled gi
              (_, _, _, Just source) -> Card.MkCue (["choosable"] <> ["auto-next" | auto request == Just source]) (MkAnswer source) Nothing
              _ -> Card.MkCue ["playable" | Set.member oid acting] (MkRead oid) Nothing
      -- Every tile previews its sheet under a resting pointer, and opens it
      -- on a long press, which leaves a pick it toggles unmade.
      previewing o = (objectCue o) {Card.peek = Just (MkPeek (ObjectView.object o), MkUnpeek, MkRead (ObjectView.object o))}
   in Board.MkMarks previewing (\pid -> fmap toggled (Map.lookup pid players))

withMark :: Card.Cue msg -> [String] -> Card.Cue msg
withMark cue extra = cue {Card.marks = Card.marks cue <> extra}

-- | A block declaration's blockers and attackers, by id (CR 509.1a).
blocks :: Request.Request -> Maybe ([ObjectId.ObjectId], [ObjectId.ObjectId])
blocks request = do
  Monad.guard $ Question.kind (Request.question request) == Text.pack "DeclareBlockers"
  let offer = Question.offer (Request.question request)
  blockers <- Protocol.field "blockers" offer >>= Protocol.arrayOf >>= traverse Offer.objectOf
  attackers <- Protocol.field "attackers" offer >>= Protocol.arrayOf >>= traverse Offer.objectOf
  pure (blockers, attackers)

-- | Before a seat's question when the screen last showed another seat's,
-- saying what the seat is asked (asking) and whom it decides for while it
-- controls them (CR 724.1); the settings at the top right, as every other
-- screen has them.
handOff :: Request.Request -> Html.Html Event
handOff request =
  Element.el
    "main"
    ["handoff"]
    []
    [ Element.el "div" ["handoff-bar"] [] [Bar.settingsButton Icon.MkGear "Settings" Nothing Nothing],
      Element.el "p" [] [] [Html.text $ Text.pack "Pass the screen to " <> Wording.player seat <> Text.pack " to " <> asking (Question.kind (Request.question request)) <> foldMap (\p -> Text.pack ", deciding for " <> Wording.player p) (Question.for (Request.question request)) <> Text.pack "."],
      primary MkReady $ Text.pack "Ready"
    ]
  where
    seat = Question.seat (Request.question request)

-- | What a seat is asked, as a verb phrase, said where every player sees
-- it (handOff): "decide" for a question asked only of a hidden card, which
-- its asking alone would reveal.
asking :: Text.Text -> Text.Text
asking kind
  | Text.unpack kind `elem` ["ChooseCompanion", "MulliganAction", "OpeningHandAction", "OfferedMiracleReveal"] = Text.pack "decide"
  | otherwise = verbOf kind

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
  "ChooseToPay" -> Text.pack "decide whether to pay"
  "ChooseVote" -> Text.pack "vote"
  "Bottom" -> Text.pack "put cards on the bottom"
  _ -> Wording.spaced kind

-- | Whether the screen is handed to the seat asked before its question
-- (handOff): it last faced another seat, unless the one person at the
-- screen plays both, and so hides neither's hand (`yours`).
handsOver :: Set.Set PlayerId.PlayerId -> Maybe PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
handsOver yours facing seat = facing /= Just seat && not (Set.member seat yours && any (`Set.member` yours) facing)

-- | What an event does, the options picked so far being `picked`.
step :: Request.Request -> Set.Set (Natural, Natural) -> Event -> Text.Text -> Outcome
step request picked event typed = case event of
  MkAnswer reply -> MkReplied reply
  MkToggle g i
    -- An item placed is taken out again, those after it moving up.
    | Just _ <- ordering request ->
        let placed = fmap snd (Set.toAscList picked)
         in MkPicking . Set.fromList . zip [0 ..] $ if i `elem` placed then filter (/= i) placed else placed <> [i]
    -- A card put where it is taken out again; put in the other pile, it
    -- moves there, last.
    | Just _ <- piles request ->
        let placed = [(k `mod` 2, j) | (k, j) <- Set.toAscList picked]
            others = filter ((/= i) . snd) placed
            kept = if (g, i) `elem` placed then others else others <> [(g, i)]
         in MkPicking $ Set.fromList [(n * 2 + p, j) | (n, (p, j)) <- zip [0 ..] kept]
    | Set.member (g, i) picked -> MkPicking $ Set.delete (g, i) picked
    | Just True <- exclusive <$> (picker request >>= \chooser -> Maybe.listToMaybe (List.genericDrop g (groups chooser))) -> MkPicking . Set.insert (g, i) $ Set.filter ((/= g) . fst) picked
    | otherwise -> MkPicking $ Set.insert (g, i) picked
  -- What is not placed follows what is, in the order offered: an ordering's
  -- items last, a pile's cards on top below those placed there.
  MkSubmit
    | Just (_, items) <- ordering request ->
        let placed = fmap snd (Set.toAscList picked)
         in MkReplied . R.Array . fmap (R.Number . toInteger) $ placed <> filter (`notElem` placed) [0 .. Natural.length items - 1]
    | Just (_, from) <- piles request ->
        let placed = Set.toAscList picked
            rest = [j | j <- [0 .. Natural.length from - 1], j `notElem` fmap snd placed]
            pile js = R.Array (fmap (List.genericIndex from) js)
         in MkReplied $ R.Array [pile [j | (k, j) <- placed, odd k], pile ([j | (k, j) <- placed, even k] <> rest)]
  MkSubmit -> case picker request of
    Just chooser ->
      let chosen = [[reply | (i, (reply, _)) <- zip [0 ..] (options group), Set.member (g, i) picked] | (g, group) <- zip [0 ..] (groups chooser)]
          wrong = [(group, Natural.length picks) | (group, picks) <- zip (groups chooser) chosen, Natural.length picks < least group || Natural.length picks > most group]
       in case wrong of
            -- The heading states the question; a lone group's refusal says
            -- only what is wrong with the picks.
            (group, count) : _
              | [_] <- groups chooser -> MkRefused (Text.pack (misPicked group count))
              | otherwise -> MkRefused $ title group <> Text.pack (": pick " <> bounds group <> ".")
            [] -> either MkRefused MkReplied $ finish chooser chosen
    Nothing -> MkPicking picked
  MkTyped
    | Question.kind (Request.question request) == Text.pack "ChooseCardName" ->
        if Text.null (Text.strip typed) then MkRefused (Text.pack "Type the name of a card.") else MkReplied . R.Text $ Text.strip typed
  MkTyped -> either (MkRefused . (Text.pack "That reply does not read: " <>)) MkReplied $ Common.parse typed >>= Codec.Reply.fromValue
  MkReady -> MkPicking picked
  -- The page leaves the game before the step (Parun.Browser.Hotseat).
  MkHome -> MkPicking picked
  MkClear -> MkPicking Set.empty
  MkRead _ -> MkPicking picked
  MkClose -> MkPicking picked
  MkFocus _ -> MkPicking picked
  MkPeek _ -> MkPicking picked
  MkUnpeek -> MkPicking picked
  MkLevel _ -> MkPicking picked
  MkConcede -> maybe (MkRefused $ Text.pack "Concede when you have priority.") (MkReplied . R.Number) $ passIndex request
  MkSkip _ -> maybe (MkRefused $ Text.pack "There is no pass to make.") (MkReplied . R.Number) $ passIndex request
  MkAuto -> maybe (MkRefused $ Text.pack "Auto can't pay the rest of this cost. Choose a source.") MkReplied $ auto request

-- | What is wrong with a group's count of picks, outside its bounds: too
-- few ("Pick 1 first.", "Pick at least 2 first.") or too many ("Pick only
-- 1.", "Pick at most 2.").
misPicked :: Group -> Natural -> String
misPicked group count
  | count < least group = "Pick " <> (if least group == most group then "" else "at least ") <> show (least group) <> " first."
  | otherwise = "Pick " <> (if least group == most group then "only " else "at most ") <> show (most group) <> "."

bounds :: Group -> String
bounds group
  | least group == most group = show (least group)
  | otherwise = "from " <> show (least group) <> " to " <> show (most group)

-- | The groups a question is answered by picking among, for the questions
-- that are: cards to put on the bottom or discard (CR 103.5, 701.9),
-- attackers (CR 508.1a), blocks (CR 509.1a), targets (CR 601.2c), cards
-- found by a search (CR 701.23b), a division among targets (CR 601.2d), life
-- totals redistributed, words a text change swaps (CR 612.2), creatures into
-- piles (CR 700.3), a set from an offer's "among" that picks no option twice,
-- an amount per item of an offer's "amounts", mana to spend, and combat
-- damage divided (CR 510.1c).
picker :: Request.Request -> Maybe Picker
picker request = do
  shown <- Request.view request
  let named :: [R.Reply] -> [(R.Reply, Text.Text)]
      named = fmap (\option -> (option, Offer.listedName shown option))
      every items = (0, Natural.length items)
  case Text.unpack (Question.kind question) of
    kind | kind `elem` ["Bottom", "ChooseDiscard"] -> do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      wanted <- min (Natural.length from) <$> (Protocol.field "count" offer >>= Protocol.naturalOf)
      let purpose = if kind == "Bottom" then " to put on the bottom of your library" else " to discard"
      pure $ MkPicker [MkGroup (Text.pack "Choose " <> Wording.count wanted "card" <> Text.pack purpose) (named from) wanted wanted False] (Right . R.Array . concat) (Text.pack "Done")
    "DeclareAttackers" -> do
      attackers <- Protocol.arrayOf offer
      pure $ MkPicker [uncurry (MkGroup (Text.pack "Choose attackers") (named attackers)) (every attackers) False] (Right . R.Array . concat) (Text.pack "Attack")
    "DeclareBlockers" -> do
      blockers <- Protocol.field "blockers" offer >>= Protocol.arrayOf
      attackers <- Protocol.field "attackers" offer >>= Protocol.arrayOf
      pure $
        MkPicker
          [uncurry (MkGroup (Offer.listedName shown blocker <> Text.pack " blocks") (named attackers)) (every attackers) False | blocker <- blockers]
          (\chosen -> Right $ R.Array [R.Array [blocker, R.Array blocked] | (blocker, blocked) <- zip blockers chosen, not (null blocked)])
          (Text.pack "Block")
    "ChooseTargets" -> do
      (source, slots) <- Offer.slotsOf question
      pure $
        MkPicker
          [ let legal = Offer.bySeat shown offered
                n = min wanted (Natural.length legal)
             in MkGroup (Text.pack "Choose " <> Wording.count n "target" <> Text.pack " for " <> Offer.objectName shown source <> foldMap (\t -> Text.pack " (" <> Wording.spaced t <> Text.pack ")") (Monad.mfilter (/= Text.pack "target") (Offer.textOf slot))) [(option, asSeat shown (Offer.recipientPlayer option) (Offer.targetName shown legal option)) | option <- legal] n n False
          | (slot, wanted, offered) <- slots
          ]
          (\chosen -> Right $ R.Array [R.Array [slot, R.Array picks] | ((slot, _, _), picks) <- zip slots chosen])
          (Text.pack "Done")
    "Search" -> do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      names <- Protocol.field "names" offer >>= Protocol.arrayOf >>= traverse Offer.textOf
      wanted <- min (Natural.length from) <$> (Protocol.field "count" offer >>= Protocol.naturalOf)
      pure $ MkPicker [MkGroup (Text.pack "Choose up to " <> Wording.count wanted "card" <> Text.pack " to find") (zip from names) 0 wanted False] (Right . R.Array . concat) (Text.pack "Find")
    -- CR 601.2d: an amount of at least one per target, adding up to the
    -- total, or using exactly the parts given.
    "ChooseDivision" -> do
      targets <- Protocol.field "targets" offer >>= Protocol.arrayOf
      let parts = Protocol.field "parts" offer >>= Protocol.arrayOf >>= mapM Protocol.naturalOf
      total <- maybe (Protocol.field "total" offer >>= Protocol.naturalOf) (Just . sum) parts
      let self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)
      pure $
        MkPicker
          [MkGroup (asSeat shown (Offer.recipientPlayer target) (Offer.recipientName shown target)) [(R.Number (toInteger n), Text.pack (show n)) | n <- [1 .. total]] 1 1 True | target <- targets]
          ( \chosen ->
              let given = [n | [R.Number n] <- chosen]
               in if sum given == toInteger total && all (\ps -> List.sort given == List.sort (fmap toInteger ps)) parts
                    then Right (R.Array [R.Array [target, R.Number n] | (target, n) <- zip targets given])
                    else Left (Text.pack "Divide " <> self <> Text.pack ("'s " <> show total <> " so it adds up" <> maybe "" (\ps -> ", using exactly " <> unwords (fmap show ps)) parts <> "."))
          )
          (Text.pack "Divide")
    -- Reverse the Sands: for each player, the player whose life total they take.
    "ChooseRedistribution" -> do
      totals <- Protocol.field "totals" offer >>= Protocol.arrayOf >>= mapM (\total -> case total of R.Array [p, R.Number life] -> Just (p, life); _ -> Nothing)
      let players = fmap fst totals
      pure $
        MkPicker
          [MkGroup (if Offer.playerOf p == Just (PlayerView.viewer shown) then Text.pack "You take the life total of" else Offer.playerName p <> Text.pack " takes the life total of") [(giver, asSeat shown (Offer.playerOf giver) (Offer.playerName giver) <> Text.pack (" (" <> show life <> ")")) | (giver, life) <- totals] 1 1 True | p <- players]
          ( \chosen ->
              let givers = concat chosen
               in if length givers == length players && all (`elem` givers) players
                    then Right (R.Array [R.Array [p, giver] | (p, giver) <- zip players givers, giver /= p])
                    else Left (Text.pack "Give each life total to exactly one player.")
          )
          (Text.pack "Redistribute")
    -- CR 612.2: the word to change, and the word it becomes.
    "ChooseWordSwap" -> do
      (from, to) <- Offer.wordSwap offer
      pure $
        MkPicker
          [MkGroup (Text.pack "Change the word") from 1 1 True, MkGroup (Text.pack "to") to 1 1 True]
          ( \chosen -> case chosen of
              [[a], [b]]
                | a == b -> Left (Text.pack "Choose two different words.")
                | Offer.swapAllowed offer a b -> Right (R.Array [a, b])
              _ -> Left (Text.pack "Choose two words of the same kind, the new one allowed.")
          )
          (Text.pack "Change")
    -- CR 700.3: each creature into one of the piles, any of them empty.
    "DividePiles" -> do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      count <- Protocol.field "count" offer >>= Protocol.naturalOf
      pure $
        MkPicker
          [MkGroup (Offer.listedName shown creature) [(R.Number (toInteger k), Text.pack ("Pile " <> show k)) | k <- [1 .. count]] 1 1 True | creature <- from]
          (\chosen -> Right (R.Array [R.Array [creature | (creature, [R.Number k']) <- zip from chosen, k' == toInteger k] | k <- [1 .. count]]))
          (Text.pack "Done")
    _
      | Just among <- Protocol.field "among" offer >>= Protocol.arrayOf,
        Protocol.field "repeats" offer /= Just (R.Boolean True) -> do
          let least' = Maybe.fromMaybe 0 $ Protocol.field "least" offer >>= Protocol.naturalOf
              most' = Maybe.fromMaybe (Natural.length among) $ Protocol.field "most" offer >>= Protocol.naturalOf
              -- An option the offer names under "names" (a mode's text, a card
              -- outside the game) by that name.
              names = foldMap (fmap Offer.textOf) (Protocol.field "names" offer >>= Protocol.arrayOf) <> repeat Nothing
          pure $ MkPicker [MkGroup (Maybe.fromMaybe (capitalized (Question.kind question)) (headingOf shown request)) [(option, Maybe.fromMaybe (Offer.optionName shown option) name) | (option, name) <- zip among names] least' most' False] (Right . R.Array . concat) (Text.pack "Done")
    -- An amount per item the offer lists, each within its range and in all
    -- within the offer's bounds (Protocol.amountsFit).
    _
      | Just caps <- Protocol.field "amounts" offer -> do
          let leaves = Protocol.amountLeaves caps
              entries = foldMap (fmap Just) (Protocol.field "entries" offer >>= Protocol.arrayOf) <> repeat Nothing
              stepName key = case key of
                Left i -> maybe (Text.pack ("Item " <> show (i + 1))) (Offer.optionName shown) (Monad.join (Maybe.listToMaybe (List.genericDrop i entries)))
                Right k -> Offer.amountKeyName shown k
          pure $
            MkPicker
              [MkGroup (maybe Text.empty stepName (Maybe.listToMaybe (reverse path))) [(R.Number (toInteger n), Text.pack (show n)) | n <- [0 .. cap]] 0 1 True | (path, cap) <- leaves]
              ( \chosen ->
                  let typed :: [Natural]
                      typed = [maybe 0 Integer.toNaturalSaturating (Maybe.listToMaybe [n | R.Number n <- picks]) | picks <- chosen]
                   in if Protocol.amountsFit offer typed then Right (Offer.fillAmounts caps typed) else Left (Text.pack "Choose amounts" <> totalBounds offer <> Text.pack ".")
              )
              (Text.pack "Done")
    "ChooseManaToSpend" -> do
      units <- Protocol.arrayOf offer
      shownUnits <- traverse (Offer.decoded Codec.ManaUnit.codec) units
      pure $ MkPicker [MkGroup (Text.pack "Choose the mana to spend") (zip units $ fmap Offer.unitName shownUnits) 1 1 True] (Right . Maybe.fromMaybe R.Null . Maybe.listToMaybe . concat) (Text.pack "Spend")
    "AssignCombatDamage" -> do
      (source, lethal, total) <- damage
      let amounts = [(R.Number $ toInteger n, Text.pack $ show n) | n <- [0 .. total]]
      pure $
        MkPicker
          [MkGroup (asSeat shown (Offer.recipientPlayer recipient) (Offer.recipientName shown recipient) <> (if threshold > 0 then Text.pack (" (lethal " <> show threshold <> ")") else Text.empty)) amounts 1 1 True | (recipient, threshold) <- lethal]
          ( \chosen ->
              let given = [n | [R.Number n] <- chosen]
               in if sum given == toInteger total
                    then Right $ R.Array [R.Array [recipient, R.Number n] | ((recipient, _), n) <- zip lethal given, n > 0]
                    else Left . Text.pack $ "Divide " <> Text.unpack (Offer.objectName shown source) <> "'s damage so it adds up to " <> show total <> "."
          )
          (Text.pack "Assign")
    _ -> Nothing
  where
    question = Request.question request
    offer = Question.offer question
    damage = do
      source <- Protocol.field "source" offer
      lethal <- Protocol.field "lethal" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf
      total <- Protocol.field "total" offer >>= Protocol.naturalOf
      pure (source, lethal, total)

-- | What an amount-per-item offer's amounts must add up to.
totalBounds :: R.Reply -> Text.Text
totalBounds offer = Text.pack $ case (Protocol.field "least" offer >>= Protocol.naturalOf, Protocol.field "most" offer >>= Protocol.naturalOf) of
  (Just l, Just m) | l == m -> " adding up to exactly " <> show m
  (Just l, Just m) -> " adding up to from " <> show l <> " to " <> show m
  (Just l, Nothing) -> " adding up to at least " <> show l
  (Nothing, Just m) -> " adding up to at most " <> show m
  (Nothing, Nothing) -> ""

-- | Whether a question is answered with an amount per row (amountForm).
amounted :: Request.Request -> Bool
amounted request =
  let question = Request.question request
   in Text.unpack (Question.kind question) `elem` ["AssignCombatDamage", "ChooseDivision"] || Maybe.isJust (Protocol.field "amounts" (Question.offer question))

-- | What a question orders, first to last, answered as a permutation of its
-- items' indices (Parun.Engine.Game.permute): triggered abilities put on the
-- stack at once (CR 603.3b), and any offer listing an "order".
ordering :: Request.Request -> Maybe (Text.Text, [Text.Text])
ordering request = do
  shown <- Request.view request
  case Text.unpack (Question.kind question) of
    "OrderTriggers" -> (,) (Text.pack "Order your triggered abilities, first to last. The last resolves first.") . fmap (Offer.triggerName shown) <$> Protocol.arrayOf offer
    -- CR 404.3: the cards going to the graveyard together, from the new top down.
    "ArrangeGraveyardArrivals" -> (,) (Text.pack "These cards go to your graveyard together. Order them from the new top down.") . fmap (Offer.listedName shown) <$> (Protocol.field "from" offer >>= Protocol.arrayOf)
    _ -> do
      items <- Protocol.field "order" offer >>= Protocol.arrayOf
      let self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)
          -- CR 601.2h: a cost's components as Oracle text prints each.
          named = if Question.kind question == Text.pack "OrderCostComponents" then Offer.componentName self else Offer.optionName shown
      pure (capitalized (Question.kind question) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer) <> Text.pack ", first to last: " <> Offer.orderNote (Question.kind question) offer <> Text.pack ".", fmap named items)
  where
    question = Request.question request
    offer = Question.offer question

-- | A scry, surveil or fateseal: its heading, the other pile's name, and the
-- cards, each placed on top or in the other pile in the order chosen, sent
-- as (other, top) (CR 701.22a, 701.25a, 701.29a).
piles :: Request.Request -> Maybe ((Text.Text, Text.Text), [R.Reply])
piles request = do
  (what, other, whither) <- case Text.unpack (Question.kind question) of
    "ChooseScry" -> Just ("Scry", "Bottom", "on the top or the bottom of your library")
    "ChooseSurveil" -> Just ("Surveil", "Graveyard", "on top of your library or into your graveyard")
    "ChooseFateseal" -> Just ("Fateseal", "Bottom", "on the top or the bottom of the library")
    _ -> Nothing
  from <- Protocol.field "from" (Question.offer question) >>= Protocol.arrayOf
  pure ((Text.pack (what <> " " <> show (length from) <> ": put each card " <> whither <> "."), Text.pack other), from)
  where
    question = Request.question request

-- | A kind's words, the first capitalized: ArrangeLibraryCards as "Arrange library cards".
capitalized :: Text.Text -> Text.Text
capitalized kind = case Text.uncons (Wording.spaced kind) of
  Just (c, rest) -> Text.cons (Char.toUpper c) rest
  Nothing -> Text.empty

-- | Whether the request has no form of its own, and is answered by a reply
-- typed as JSON.
answersTyped :: Request.Request -> Bool
answersTyped request = fmap Html.toText (form request Map.empty Set.empty Nothing) == fmap Html.toText (typedReply request)

-- | The offer shown as it is sent, and a field for a reply typed as JSON.
typedReply :: Request.Request -> [Html.Html Event]
typedReply request =
  [ Html.element (Text.pack "h2") [] [Html.text (Wording.spaced (Question.kind question) <> Text.pack ":")],
    Html.element (Text.pack "pre") [] [Html.text . Common.render $ Codec.Reply.toValue (Question.offer question)],
    Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "Reply as JSON"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []
  ]
  where
    question = Request.question request

form :: Request.Request -> Map.Map ObjectId.ObjectId [Text.Text] -> Set.Set (Natural, Natural) -> Maybe Natural -> [Html.Html Event]
form request texts picked focused = case Text.unpack (Question.kind question) of
  "PlayOrDraw" -> heading "Play first or draw?" : [button (MkAnswer option) (Offer.tagName word) | option@(R.Text word) <- Foldable.fold (Protocol.arrayOf offer)]
  -- CR 103.1: among three or more seats, the seat to take the first turn.
  "ChooseStarter" -> heading "Who takes the first turn?" : [button (MkAnswer option) (if option == R.Text (Text.pack (show (PlayerId.unwrap (Question.seat question)))) then Text.pack "You" else Offer.playerName option) | option <- Foldable.fold (Protocol.arrayOf offer)]
  "DeclareMulligan" ->
    heading ("Keep this hand, or mulligan?" <> foldMap (\n -> " A mulligan puts " <> Text.unpack (Wording.count n "card") <> " on the bottom.") (Protocol.field "bottom" offer >>= Protocol.naturalOf))
      : [primary (MkAnswer . R.Text $ Text.pack "Keep") (Text.pack "Keep"), button (MkAnswer . R.Text $ Text.pack "Mulligan") (Text.pack "Mulligan")]
  "ChooseManaToSpend"
    | Just units <- Protocol.arrayOf offer,
      Just shownUnits <- traverse (Offer.decoded Codec.ManaUnit.codec) units ->
        heading "Choose the mana to spend." : [button (MkAnswer unit) (Offer.unitName shown) | (unit, shown) <- zip units shownUnits]
  "ChooseAction"
    | Just shown <- Request.view request,
      Just options' <- Protocol.arrayOf offer ->
        let (passes, others) = List.partition (isPass . snd) (Offer.distinct shown options')
            -- A mana ability is in its permanent's sheet, not the list.
            rest = filter (not . isManaAbility . snd) others
            controlled = Question.for question
            -- CR 724.1: a turn the seat controls is played as its own.
            yours = PlayerView.active shown == PlayerView.viewer shown || controlled == Just (PlayerView.active shown)
            settled = null (PlayerView.stack shown)
            turn = PlayerView.turn shown
            -- Where in this turn the seat may ask to pass to; its own combat
            -- starts with its declaration, which ends a skip anyway.
            steps =
              [(Phase.Combat CombatStep.BeginningOfCombat, "Pass to their combat") | not yours]
                <> [(Phase.Ending EndingStep.EndStep, if yours then "Pass to end step" else "Pass to their end step")]
            prompt
              | not settled = "Respond, or pass to let the top of the stack resolve."
              -- CR 509: standard stops here to show the blocks (blocksShown).
              | PlayerView.phase shown == Phase.Combat CombatStep.DeclareBlockers, not (Map.null (PlayerView.blockers shown)) = "Blocks are declared. Respond, or pass to go on to combat damage."
              | Just p <- controlled, yours = Text.unpack (Wording.player p) <> "'s turn, which you control. Play something for them, or pass to move on."
              | yours = "Your turn. Play something, or pass to move on."
              | otherwise = "Their turn. Respond, or pass."
            -- Unspent mana empties as the step ends (CR 106.4, 500.5) unless
            -- it is kept longer.
            floating = [u | seat <- PlayerView.seats shown, SeatView.player seat == PlayerView.viewer shown, u <- Mana.Type.unwrap (SeatView.manaPool seat), ManaUnit.retention u == ManaRetention.Ordinary]
            nudges =
              [ nudge (Text.pack "You have " <> foldMap (\u -> Text.pack "{" <> Wording.manaType (ManaUnit.manaType u) <> Text.pack "}") floating <> Text.pack " unspent. It empties when this step ends.")
              | not (null floating)
              ]
            -- What passing gives up, the offer's `lapses`, under a heading of
            -- its own above the rest, so the buttons are the reminder; passing
            -- to the turn's end gives up at least as much.
            (lapsing, kept) = if yours && settled then List.partition (lapses . snd) rest else ([], rest)
            actionGroup title' group =
              [Element.el "h3" ["subhead"] [] [Element.string title'] | not (null lapsing), not (null group)]
                <> [Element.el "div" ["actions"] [] [button (MkAnswer $ R.Number i) (actionLabel texts shown option) | (i, option) <- group] | not (null group)]
         in heading prompt
              : combatPairs shown
                <> nudges
                <> actionGroup "Gone once you pass or end the turn" lapsing
                <> actionGroup "Still yours after you pass" kept
                <> [ Element.el "div" ["passes"] [] $
                       [primary (MkAnswer $ R.Number i) (Text.pack (if settled then "Pass" else "Pass and resolve")) | (i, _) <- passes]
                         <> [shortcut ["turn"] "E" (MkSkip (MkTurnEnds turn)) (Text.pack (if yours then "End turn" else "Pass to my turn")) | settled, not (null passes)]
                   ]
                <> [ Element.el "div" ["until"] [] $
                       [Element.button ["quiet"] (MkSkip (MkStep turn target)) [Element.string label] | (target, label) <- steps, PlayerView.phase shown < target]
                         <> [Element.button ["quiet"] (MkSkip MkStacked) [Element.string "Pass until a spell or ability"]]
                   | settled,
                     not (null passes)
                   ]
  kind
    | kind `elem` ["ChooseManaSource", "ChooseExtraManaSource"],
      Just shown <- Request.view request,
      Just sources <- Protocol.field "sources" offer >>= Protocol.arrayOf >>= traverse Offer.manaSource ->
        Html.element (Text.pack "h2") [] (Mana.rich $ foldMap (\cost -> Text.pack "Paying " <> Wording.manaCost cost <> Text.pack ". ") (Protocol.field "cost" offer >>= Offer.decoded Codec.ManaCost.codec) <> Text.pack (if kind == "ChooseManaSource" then "Choose a source." else "The cost is covered. Activate another source, or pay now."))
          -- With one source, paying automatically is tapping it.
          : [primary MkAuto (Text.pack "Pay automatically") | Maybe.isJust (auto request), length sources > 1]
            -- Sources the seat can't tell apart are one button, for the first.
            <> [button (MkAnswer source) (label <> many count) | (source : _, label, count) <- collapsed (const True) [(source, Offer.listedName shown source <> makes yields) | (source, yields) <- sources]]
            -- CR 734.1: where stopping takes the cast back, it is its Cancel.
            <> [ if kind /= "ChooseManaSource"
                   then primary (MkAnswer R.Null) (Text.pack "Pay now")
                   else maybe (button (MkAnswer R.Null) (Text.pack "Stop paying")) cancel (Question.withdraw question)
               ]
  kind
    | kind `elem` ["ChooseOptional", "ReverseManaAbilities"],
      Just shown <- Request.view request ->
        heading
          ( Text.unpack $
              if kind == "ChooseOptional"
                then foldMap (Offer.objectName shown) (Protocol.field "source" offer) <> Text.pack ": use its optional effect?"
                else Text.pack "That action was reversed. Reverse the mana ability of " <> Text.intercalate (Text.pack " and ") (foldMap (fmap (Offer.listedName shown)) (Protocol.field "sources" offer >>= Protocol.arrayOf)) <> Text.pack " too?"
          )
          -- The cards an optional effect asked right after a look is about.
          : [looking shown looked (const Nothing) | kind == "ChooseOptional", Just (looked, True) <- [latestLook shown]]
            <> [button (MkAnswer . R.Text $ Text.pack tag) (Text.pack label) | (tag, label) <- [("Exercises", "Yes"), ("Declines", "No")]]
  kind
    | Just shown <- Request.view request,
      Nothing <- Protocol.field "among" offer,
      Nothing <- Protocol.field "amounts" offer,
      Just least' <- Protocol.field "least" offer >>= Protocol.naturalOf,
      Maybe.isNothing (Protocol.field "options" offer),
      kind `notElem` ["ChooseDistribution", "ChooseExilesTotalling"] ->
        let greatest = Protocol.field "greatest" offer >>= Protocol.naturalOf
            self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)
            what
              | kind == "ChooseX" = Text.pack "Choose " <> Foldable.fold (Protocol.field "variable" offer >>= Offer.textOf) <> Text.pack " for " <> self
              -- CR 601.2b: how many times the keyword's cost is paid.
              | kind == "ChooseKicker" = Text.pack "How many times do you pay " <> Maybe.fromMaybe (Text.pack "its cost") (Protocol.field "keyword" offer >>= Offer.decoded Codec.Keyword.codec >>= Maybe.listToMaybe . Oracle.Keyword.printed self) <> Text.pack " for " <> self <> Text.pack "?"
              | kind == "ChooseNumber" = Text.pack "Choose a number" <> (if Text.null self then Text.empty else Text.pack " for " <> self)
              | otherwise = capitalized (Question.kind question) <> (if Text.null self then Text.empty else Text.pack " for " <> self)
            range = maybe (" (at least " <> show least' <> ")") (\g -> " (" <> show least' <> " to " <> show g <> ")") greatest
            -- With no greatest, the first few numbers, and any other typed.
            shownTo = Maybe.fromMaybe (least' + 9) greatest
         in heading (Text.unpack what <> range)
              : ( if shownTo - least' <= 20
                    then [Element.el "div" ["actions", "numbers"] [] [button (MkAnswer . R.Number $ toInteger n) (Text.pack $ show n) | n <- [least' .. shownTo]]] <> [numberInput | Maybe.isNothing greatest]
                    else [numberInput]
                )
                <> [Element.el "div" ["passes"] [] [cancel withdrawal] | Just withdrawal <- [Question.withdraw question]]
  _
    | Just (title', items) <- ordering request ->
        Html.element (Text.pack "h2") [] (Mana.rich title')
          : [ let place = List.find ((== i) . snd) (Set.toList picked)
               in pressable [] [] (pressedIf (Maybe.isJust place)) (MkToggle 0 i) (Mana.rich (foldMap (\(k, _) -> Text.pack (show (k + 1) <> ". ")) place <> label))
            | (i, label) <- zip [0 ..] items
            ]
            <> [Element.el "div" ["passes"] [] $ primary MkSubmit (Text.pack (if Set.null picked then "Keep this order" else "Done")) : [button MkClear (Text.pack "Start over") | not (Set.null picked)]]
  _
    | Just ((title', other), from) <- piles request,
      Just shown <- Request.view request ->
        let placed = [(k `mod` 2, j) | (k, j) <- Set.toAscList picked]
            -- The card's place in its pile, counted from 1 in the order chosen.
            at j = lookup j [(j', (p, n)) | p <- [0, 1], (n, j') <- zip [1 :: Int ..] [c | (p', c) <- placed, p' == p]]
            choice j p label =
              let on = fmap fst (at j) == Just p
               in pressable
                    ["on" | on]
                    []
                    (pressedIf on)
                    (MkToggle p j)
                    [Element.text (label <> (if on then foldMap (\(_, n) -> Text.pack (" " <> show n)) (at j) else Text.empty))]
            row j card =
              let name = Offer.listedName shown card
               in Element.el
                    "div"
                    ["amount-row"]
                    [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" name]
                    [Element.el "span" ["amount-label"] [] (Mana.rich name), Element.el "div" ["segmented"] [] [choice j 0 (Text.pack "Top"), choice j 1 other]]
            order = Text.pack "Each pile takes the cards in the order you choose them: the first on top is the top card" <> (if other == Text.pack "Bottom" then Text.pack ", the last on the bottom the bottom card" else Text.empty) <> Text.pack ". Any you leave stay on top, below those."
         in Html.element (Text.pack "h2") [] [Html.text title']
              : [nudge order | length from > 1]
                <> zipWith row [0 ..] from
                <> [Element.el "div" ["passes"] [] $ primary MkSubmit (Text.pack (if Set.null picked then "Keep all on top" else "Done")) : [button MkClear (Text.pack "Start over") | not (Set.null picked)]]
  "ChooseCardName" -> [heading "Name a card.", Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "A card's name"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []]
  -- A yes or no about paying a cost (CR 118.12), Channel's life for mana
  -- (CR 116.2c), where combat damage goes (CR 510.1b-c), a guess (CR
  -- 608.2c), or a commander's move to the command zone (CR 903.9a-b).
  kind
    | Just shown <- Request.view request,
      Just (asked, yes, no) <- yesOrNo shown request ->
        Html.element (Text.pack "h2") [] (Mana.rich asked)
          : fmap nudge (if kind == "ChooseToPay" then filter (not . Text.isPrefixOf (Text.pack "Cost:")) (Offer.told shown Text.empty offer) else [])
            <> [Element.el "div" ["passes"] [] [primary (MkAnswer (R.Text (Text.pack (fst yes)))) (Text.pack (snd yes)), button (MkAnswer (R.Text (Text.pack (fst no)))) (Text.pack (snd no))]]
  kind
    | Just shown <- Request.view request,
      Just (options', name) <- Monad.mplus (fmap (\o -> (o, Offer.optionName shown)) (Protocol.field "options" offer >>= Protocol.arrayOf)) (fmap (\o -> (o, \option -> asSeat shown (Offer.playerOf option) (Offer.playerName option))) (Protocol.field "players" offer >>= Protocol.arrayOf)) ->
        let entries = foldMap (fmap Just) (Protocol.field "entries" offer >>= Protocol.arrayOf) <> repeat Nothing
            -- CR 706.4: what each result carries, in the options' order.
            carries = foldMap (fmap Just) (Protocol.field "carries" offer >>= Protocol.arrayOf) <> repeat Nothing
            -- CR 309.4: a room by its place in the dungeon, topmost first.
            label option (entry, carry)
              | kind == "ChooseRoom", Just n <- Protocol.naturalOf option = Text.pack ("Room " <> show (n + 1))
              | Just carried <- carry = Offer.resultName shown (Maybe.fromMaybe option entry) carried
              | otherwise = maybe (name option) (Offer.entryName shown) entry
            labelled = [(option, label option entry) | (option, entry) <- zip options' (zip entries carries)]
         in Html.element (Text.pack "h2") [] (Mana.rich $ Maybe.fromMaybe (capitalized (Question.kind question) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer) <> Text.pack ".") (headingOf shown request))
              : foldMap (\among -> [looking shown among (`lookup` [(oid, option) | option <- options', Just oid <- [Offer.objectOf option]])]) (amongCards shown options')
                -- Cards the seat can't tell apart share a button, for the first.
                <> [button (MkAnswer option) (named' <> many count) | (option : _, named', count) <- collapsed (Maybe.isJust . Offer.objectOf) labelled]
  "DeclareAttackers"
    | Just attackers <- Protocol.arrayOf offer,
      Just shown <- Request.view request ->
        let count = Set.size picked
         in [ heading "Declare attackers: choose the creatures to attack with.",
              Element.el "div" ["actions"] [] (toggles 0 (Natural.length attackers) [(i, Offer.listedName shown option) | (i, option) <- zip [0 ..] attackers]),
              Element.el "div" ["passes"] [] $
                [primary MkSubmit (Text.pack (if count == 0 then "No attack" else "Attack with " <> show count))]
                  <> [shortcut [] "A" (MkAnswer (R.Array attackers)) (Text.pack "Attack with all") | count < length attackers]
            ]
  kind
    | amounted request || kind `elem` ["ChooseRedistribution", "DividePiles"] || (kind == "ChooseWordSwap" && maybe False (all ((<= 12) . length . options) . groups) (picker request)),
      Just shown <- Request.view request,
      Just chooser <- picker request ->
        let self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)
            named key = foldMap (Offer.objectName shown) (Protocol.field key offer)
            for' = if Text.null self then Text.empty else Text.pack " for " <> self
            title' = case kind of
              "AssignCombatDamage" -> Text.pack "Divide the combat damage."
              "ChooseDivision" -> Text.pack "Divide among the targets of " <> self <> Text.pack ", at least 1 each" <> foldMap (\n -> Text.pack (" (" <> show n <> " in all)")) (Protocol.field "total" offer >>= Protocol.naturalOf) <> Text.pack "."
              "ChooseRedistribution" -> Text.pack "Redistribute the life totals."
              "DividePiles" -> Text.pack "Divide the creatures into piles."
              "ChooseWordSwap" -> Text.pack "Choose the words " <> self <> Text.pack " swaps."
              "AllocateDamage" -> Text.pack "Choose how much of each damage to prevent" <> inAll <> Text.pack "."
              "AnnounceTargets" -> Text.pack "How many targets for " <> self <> Text.pack "?"
              "ChooseStoredRerolls" -> Text.pack "Choose the stored results to reroll" <> for' <> Text.pack "."
              "ChooseDistribution" -> Text.pack "Divide" <> for' <> inAll <> Text.pack "."
              _
                | Maybe.isJust (Protocol.field "off" offer) -> Text.pack "Move counters from " <> named "off" <> foldMap (\_ -> Text.pack " onto " <> named "onto") (Protocol.field "onto" offer) <> inAll <> Text.pack "."
                | Text.isInfixOf (Text.pack "CounterRemoval") (Question.kind question) -> Text.pack "Choose counters to remove" <> for' <> inAll <> Text.pack "."
                | otherwise -> capitalized (Question.kind question) <> for' <> inAll <> Text.pack "."
            inAll = case totalBounds offer of
              b | Text.null b -> Text.empty
              b -> Text.pack "," <> b
            context = if Maybe.isJust (Protocol.field "amounts" offer) then filter (\line -> not (any ((`Text.isPrefixOf` line) . Text.pack) ["Off:", "Onto:"])) (Offer.told shown self offer) else []
            -- Nested amounts (a kind of counter on each permanent) under a
            -- heading per permanent.
            sections = case Protocol.field "amounts" offer of
              Just caps -> [case path of Right k : _ : _ -> Just (Offer.amountKeyName shown k); _ -> Nothing | (path, _) <- Protocol.amountLeaves caps]
              Nothing -> []
            sectionOf g = Monad.join (Maybe.listToMaybe (drop g sections))
            row g group =
              let on i = Set.member (Int.toNaturalSaturating g, i) picked
               in Element.el
                    "div"
                    ["amount-row"]
                    [Element.attr "role" (Text.pack "group"), Element.attr "aria-label" (foldMap Html.spoken (Mana.rich (title group)))]
                    [ Element.el "span" ["amount-label"] [] (Mana.rich (title group)),
                      Element.el "div" ["segmented"] [] [pressable ["on" | on i] [] (pressedIf (on i)) (MkToggle (Int.toNaturalSaturating g) i) (Mana.rich label) | (i, (_, label)) <- zip [0 ..] (options group)]
                    ]
            rows = concat [[Element.el "h3" ["subhead"] [] (Mana.rich t) | Just t <- [sectionOf g], g == 0 || sectionOf (g - 1) /= Just t] <> [row g group] | (g, group) <- zip [0 ..] (groups chooser)]
         in Html.element (Text.pack "h2") [] (Mana.rich title')
              : fmap nudge context
                <> rows
                <> [primary MkSubmit (send chooser)]
                <> [cancel withdrawal | Just withdrawal <- [Question.withdraw question]]
  -- One of several yields, shares or costs, answered by its index.
  kind
    | Just shown <- Request.view request,
      Just (asked, labels) <- indexed shown request ->
        Html.element (Text.pack "h2") [] (Mana.rich asked)
          : fmap nudge (if kind == "ChooseCost" then Offer.told shown (foldMap (Offer.objectName shown) (Protocol.field "source" offer)) offer else [])
            <> [Element.el "div" ["actions"] [] [button (MkAnswer (R.Number i)) label | (i, label) <- zip [0 ..] labels]]
            <> [Element.el "div" ["passes"] [] [cancel withdrawal] | Just withdrawal <- [Question.withdraw question]]
  "DeclareBlockers"
    | Just chooser <- picker request ->
        heading "Declare blockers: choose a creature of yours, then the attacker it blocks."
          : concat
            [ ( if focused == Just g
                  then Element.el "h3" ["subhead", "focused"] [Element.attr "aria-current" (Text.pack "true")] (Mana.rich (title group <> Text.pack ": choose the attacker it blocks"))
                  else Element.el "h3" ["subhead"] [] (Mana.rich (title group))
              )
                : toggles g (most group) [(i, label) | (i, (_, label)) <- zip [0 ..] (options group)]
            | (g, group) <- zip [0 ..] (groups chooser)
            ]
            <> [primary MkSubmit (Text.pack (if Set.null picked then "No blocks" else "Block"))]
  _
    | Just chooser <- picker request ->
        concat
          [ Html.element (Text.pack "h2") [] (Mana.rich $ title group <> (if Text.any Char.isDigit (title group) then Text.empty else Text.pack (" (choose " <> bounds group <> ")")))
              : toggles g (most group) [(i, label) | (i, (_, label)) <- zip [0 ..] (options group)]
          | (g, group) <- zip [0 ..] (groups chooser)
          ]
          -- Answering waits on picks within every group's bounds, the refusal
          -- in `step` a fence behind it.
          <> [ Element.el "div" ["passes"] [] $
                 Element.el "button" ["primary"] ([Element.attr "type" (Text.pack "button"), Element.on "click" MkSubmit] <> [Element.attr "disabled" Text.empty | not (all fits (zip [0 ..] (groups chooser)))]) (Mana.rich (send chooser))
                   : [button MkClear (Text.pack "Start over") | not (Set.null picked)]
                     <> [cancel withdrawal | Just withdrawal <- [Question.withdraw question]]
             ]
  _ -> typedReply request
  where
    question = Request.question request
    offer = Question.offer question
    -- CR 734.1: takes back the spell or ability being announced.
    cancel withdrawal = button (MkAnswer withdrawal) (Text.pack "Cancel")
    -- The check mark is drawn only: aria-pressed says it.
    toggle g i label =
      let on = Set.member (g, i) picked
       in pressable [] [] (pressedIf on) (MkToggle g i) ([Element.el "span" [] [Element.attr "aria-hidden" (Text.pack "true")] [Element.string "\x2713 "] | on] <> Mana.rich label)
    -- Options the seat can't tell apart share a button: a tap picks the
    -- first unpicked while the group has room, else drops the last picked.
    toggles g room labelled =
      [ case indices of
          [i] -> toggle g i label
          _ ->
            let chosen = filter (\i -> Set.member (g, i) picked) indices
                full = Natural.length (filter ((== g) . fst) (Set.toList picked)) >= room
                next = case filter (\i -> not (Set.member (g, i) picked)) indices of
                  i : _ | null chosen || not full -> i
                  _ -> List.last chosen
             in pressable [] [] (if null chosen then "false" else if length chosen == length indices then "true" else "mixed") (MkToggle g next) $ Mana.rich ((if null chosen then Text.empty else Text.pack (show (length chosen) <> " of ")) <> label <> many (length indices))
      | (indices, label, _) <- collapsed (const True) labelled
      ]
    many n = if n > 1 then Text.pack (" \x00d7" <> show n) else Text.empty
    fits (g, group) = let n = Natural.length (filter ((== g) . fst) (Set.toList picked)) in least group <= n && n <= most group
    makes yields = if null yields then Text.empty else Text.pack " (makes " <> Text.intercalate (Text.pack " or ") (fmap (foldMap (\m -> Text.pack "{" <> Wording.manaType m <> Text.pack "}")) yields) <> Text.pack ")"
    heading t = Html.element (Text.pack "h2") [] (Mana.rich $ Text.pack t)

-- | A field to type a number into, answering with it.
numberInput :: Html.Html Event
numberInput = Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "Another number"), Attribute.MkProperty (Text.pack "inputmode") (Text.pack "numeric"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []

-- | A yes-or-no question's heading and its answers' tags and labels, yes
-- first.
yesOrNo :: PlayerView.PlayerView -> Request.Request -> Maybe (Text.Text, (String, String), (String, String))
yesOrNo shown request = case Text.unpack (Question.kind (Request.question request)) of
  "ChooseToPay" -> Just (self <> Text.pack ": pay " <> foldMap (Offer.candidateName self) (Protocol.field "cost" offer) <> Text.pack "?", ("Pays", "Pay"), ("Declines", "Don't pay"))
  "PayLifeForMana" -> do
    life <- Protocol.field "life" offer >>= Protocol.naturalOf
    mana <- Protocol.field "mana" offer >>= Offer.decoded Codec.ManaType.codec
    Just (Text.pack ("Pay " <> show life <> " life to add {") <> Wording.manaType mana <> Text.pack "}?", ("Exercises", "Pay life"), ("Declines", "Don't"))
  "ChooseCombatDamageOption" -> do
    option <- Protocol.field "option" offer >>= Offer.decoded Codec.CombatDamageOption.codec
    let wording = if option == CombatDamageOption.AsThoughUnblocked then Text.pack ": assign its combat damage as though it weren't blocked?" else Text.pack ": assign its combat damage to a creature the defending player controls?"
    Just (self <> wording, ("Exercises", "Yes"), ("Declines", "No"))
  "ChooseGuess" -> Just (self <> Text.pack ": guess whether the hidden card is as its text says.", ("Holds", "It is"), ("DoesNotHold", "It isn't"))
  "ReturnCommander" -> Just (Text.pack "Put " <> self <> Text.pack " into the command zone?", ("Returns", "Command zone"), ("Leaves", "Don't"))
  "Rethrow" -> fmap (\asked -> (asked, ("Exercises", if Maybe.isJust (Protocol.field "face" offer) then "Reflip" else "Reroll"), ("Declines", "Keep it"))) (Offer.rerollQuestion self offer)
  _ -> Nothing
  where
    offer = Question.offer (Request.question request)
    self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)

-- | A question's heading in words, where its kind's name says too little.
headingOf :: PlayerView.PlayerView -> Request.Request -> Maybe Text.Text
headingOf shown request = case Text.unpack (Question.kind question) of
  "ChooseLegend" -> Just (Text.pack "Legend rule: choose the legend to keep. The rest go to the graveyard.")
  "ChooseRingBearer" -> Just (Text.pack "The Ring tempts you: choose your Ring-bearer.")
  "ChooseRoom" -> Just (Text.pack "Venture into " <> self <> Text.pack ": choose the next room.")
  "ChooseDungeon" -> Just (Text.pack "Choose a dungeon to venture into.")
  "ChooseAttackTarget" -> Just (Text.pack "Choose what " <> self <> Text.pack " attacks.")
  "ChooseMutateSide" -> Just (Text.pack "Put " <> self <> Text.pack " over or under " <> named "onto" <> Text.pack "?")
  "ChooseModes" -> Just (Text.pack "Choose modes for " <> self)
  "ChooseVote" -> Just (Text.pack "Vote for " <> self <> Text.pack ".")
  "ChooseReplacement" -> Just (Text.pack "Choose the replacement effect that applies first.")
  -- CR 706.2b.
  "ChooseRollModifier" -> Just (Text.pack "Choose the modifier to apply to the roll next.")
  -- CR 706.4.
  "ChooseDieResult" -> Just (Text.pack "Choose the result to use.")
  "ChooseHalf" -> Just (Text.pack "Choose a door of " <> self <> Text.pack ".")
  "ChooseProliferate" -> Just (Text.pack "Proliferate: choose permanents and players to get another counter of each kind they have")
  "DeclareBands" -> Just (Text.pack "Choose attacking creatures to band together")
  "ChooseClash" -> Just (Text.pack "Clash: keep the revealed card on top of your library, or put it on the bottom?")
  "ChooseFromOutsideTheGame" -> Just (Text.pack "Choose cards from outside the game")
  "ChooseCardFromAmong" -> Just (Text.pack "Choose a card for " <> self <> Text.pack ".")
  "ChooseExert" -> Just (Text.pack "Exert " <> self <> Text.pack " as it attacks?")
  "OfferedCast" -> Just (Text.pack "Cast " <> Maybe.fromMaybe self (Protocol.field "name" offer >>= Offer.textOf) <> Text.pack "?")
  "ChoosePlayPermission" -> Just (Text.pack "Choose the permission to play " <> self <> Text.pack " by.")
  "ChooseCompanion" -> Just (Text.pack "Reveal a companion? It waits outside the game, and you may put it into your hand for {3}.")
  "OpeningHandAction" -> Just (Text.pack "Before the game begins: use a card in your opening hand?")
  "MulliganAction" -> Just (Text.pack "Before you take a mulligan: use a card in your hand?")
  "KeepHand" -> Just (Text.pack "Choose the hand to keep.")
  "ChooseForage" -> Just (Text.pack "Forage for " <> self <> Text.pack ": exile three cards from your graveyard, or sacrifice a Food?")
  _ -> Nothing
  where
    question = Request.question request
    offer = Question.offer question
    named key = foldMap (Offer.objectName shown) (Protocol.field key offer)
    self = named "source"

-- | A question answered by the index of one of its options: the mana a
-- source makes (CR 106.3), the mana spent on X (CR 107.3), or how to pay a
-- cost (CR 601.2b); its heading and each option's label.
indexed :: PlayerView.PlayerView -> Request.Request -> Maybe (Text.Text, [Text.Text])
indexed shown request = case Text.unpack (Question.kind (Request.question request)) of
  "ChooseManaYield" -> do
    yields <- Protocol.field "yields" offer >>= Offer.decoded (Common.list (Common.list Codec.ManaType.codec))
    Just (Text.pack "Choose the mana " <> self <> Text.pack " makes.", fmap symbols yields)
  "ChooseManaSpentOnX" -> do
    shares <- Protocol.field "shares" offer >>= Offer.decoded (Common.list (Common.list Codec.ManaType.codec))
    Just (Text.pack "Choose the mana you spent on X for " <> self <> Text.pack ".", fmap symbols shares)
  "ChooseCost" -> do
    costs <- Protocol.field "costs" offer >>= Protocol.arrayOf
    let keywords = foldMap (fmap (Offer.decoded Codec.Keyword.codec)) (Protocol.field "keywords" offer >>= Protocol.arrayOf) <> repeat Nothing
        named cost keyword = Offer.candidateName self cost <> foldMap (\k -> Text.pack " (" <> Wording.spaced (Text.pack (Codec.Keyword.tagOf k)) <> Text.pack ")") keyword
    Just (Text.pack "Choose how to pay for " <> self <> Text.pack ".", zipWith named costs keywords)
  _ -> Nothing
  where
    offer = Question.offer (Request.question request)
    self = foldMap (Offer.objectName shown) (Protocol.field "source" offer)
    symbols :: [ManaType.ManaType] -> Text.Text
    symbols = foldMap (\t -> Text.pack "{" <> Wording.manaType t <> Text.pack "}")

-- | Options by label, each shareable label once with every shareable option
-- bearing it, in the order labels first appear, and every other option
-- alone; the label's count is the third part.
collapsed :: (a -> Bool) -> [(a, Text.Text)] -> [([a], Text.Text, Int)]
collapsed shareable labelled =
  let keyed = [(if shareable x then Left label else Right i, (x, label)) | (i, (x, label)) <- zip [0 :: Int ..] labelled]
   in [(fmap fst xs, label, length xs) | key <- ListUtils.nubOrd (fmap fst keyed), let xs = [v | (k, v) <- keyed, k == key], (_, label) : _ <- [xs]]

-- | CR 701.20e: the cards the seat's latest look showed it, in the order
-- looked at, and whether that look is the last thing the seat saw happen;
-- Nothing where the seat saw no look.
latestLook :: PlayerView.PlayerView -> Maybe ([ObjectId.ObjectId], Bool)
latestLook shown =
  let card entry = case LoggedEvent.event entry of
        GameEvent.Looked l -> Just (Looked.card l)
        _ -> Nothing
      newest = reverse (Wording.witnessed shown)
      run = takeWhile (Maybe.isJust . card) (dropWhile (Maybe.isNothing . card) newest)
   in if null run then Nothing else Just (ListUtils.nubOrd (reverse (Maybe.mapMaybe card run)), Maybe.isJust (card =<< Maybe.listToMaybe newest))

-- | The cards a choice among cards is among, where none is on the
-- battlefield or the stack, whose tiles the table already shows: every card
-- the latest look showed, where the options are among them, else the
-- options.
amongCards :: PlayerView.PlayerView -> [R.Reply] -> Maybe [ObjectId.ObjectId]
amongCards shown options' = do
  oids <- traverse Offer.objectOf options'
  Monad.guard (not (null oids) && all (maybe True ((`notElem` [Zone.Battlefield, Zone.Stack]) . fst) . Wording.located shown) oids)
  pure $ case latestLook shown of
    Just (looked, _) | all (`elem` looked) oids -> looked
    _ -> oids

-- | Cards as tiles beside a question: each answering with the reply given
-- for it, the rest opening their sheets.
looking :: PlayerView.PlayerView -> [ObjectId.ObjectId] -> (ObjectId.ObjectId -> Maybe R.Reply) -> Html.Html Event
looking shown oids answer =
  Element.el
    "div"
    ["seen", "among"]
    []
    [ Card.tile shown (Card.MkCue (["choosable" | Maybe.isJust (answer oid)]) (maybe (MkRead oid) MkAnswer (answer oid)) (Just (MkPeek oid, MkUnpeek, MkRead oid))) o
    | oid <- oids,
      Just o <- [Wording.recalled shown oid]
    ]

-- | A label naming a player, the seat itself named as its table names it
-- (Parun.Web.Board's seats): "You (20 life)", not "Player 1 (20 life)".
asSeat :: PlayerView.PlayerView -> Maybe PlayerId.PlayerId -> Text.Text -> Text.Text
asSeat shown who label = case who of
  Just p | p == PlayerView.viewer shown, Just rest <- Text.stripPrefix (Wording.player p) label -> Text.pack "You" <> rest
  _ -> label

-- | A word of information beside a question, which answers nothing.
nudge :: Text.Text -> Html.Html msg
nudge t = Element.el "p" ["nudge"] [Element.attr "role" (Text.pack "note")] (Mana.rich t)

-- | A button that is on or off, its state read aloud as aria-pressed (a
-- string, "mixed" where it stands for several options, some on), its
-- classes and attributes besides.
pressable :: [String] -> [Attribute.Attribute Event] -> String -> Event -> [Html.Html Event] -> Html.Html Event
pressable classes attrs state event = Element.el "button" classes ([Element.attr "type" (Text.pack "button")] <> attrs <> [Element.attr "aria-pressed" (Text.pack state), Element.on "click" event])

-- | A two-state button's aria-pressed.
pressedIf :: Bool -> String
pressedIf on = if on then "true" else "false"

button :: Event -> Text.Text -> Html.Html Event
button event label = Element.button [] event (Mana.rich label)

-- | A button its key presses wherever it shows (page.js): a meaning's key,
-- the same from prompt to prompt, where page.js numbers the rest; "turn"
-- among its classes marks the one ending the turn, in the turn's colour.
shortcut :: [String] -> String -> Event -> Text.Text -> Html.Html Event
shortcut classes key event label = Element.el "button" classes [Element.attr "type" (Text.pack "button"), Element.attr "aria-keyshortcuts" (Text.pack key), Element.on "click" event] (Mana.rich label)

-- | The button that answers, set apart from the rest.
primary :: Event -> Text.Text -> Html.Html Event
primary event label = Element.button ["primary"] event (Mana.rich label)
