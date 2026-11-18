-- | A line protocol request as a page: the seat's board, what it is asked,
-- and an answer built from taps. Each kind of question a Portal game reaches
-- has its own form; any other shows its offer and takes a reply typed as JSON.
module Parun.Web.Question where

import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaUnit as Codec.ManaUnit
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Offer as Offer
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Web.Attribute as Attribute
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Element as Element
import qualified Parun.Web.Html as Html
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
  | -- | Shows a card's text (Render.details).
    MkRead ObjectId.ObjectId
  | -- | Hides the card shown.
    MkClose
  | -- | Makes this blocker the one the next attacker tapped is for.
    MkFocus Natural
  | -- | Passes now, and every priority after it until the turn ends while
    -- the stack is empty (the seat's own stop, not the engine's choice).
    MkSkip
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

-- | A reply the seat is never asked for: the hotseat never concedes unasked
-- (CR 104.3a), the seat plays first when it chooses, a priority offering nothing but a pass and mana abilities is
-- passed (Offer.onlyPass), and a choice between indistinguishable options is
-- made: identical mana to spend, and all of a creature's combat damage to
-- its one recipient (CR 510.1c).
automatic :: Request.Request -> Maybe R.Reply
automatic request = case Text.unpack (Question.kind question) of
  "Concede" -> Just . R.Text $ Text.pack "Continues"
  -- The player is taken to want to play first (CR 103.1), a default of this
  -- client's rather than the engine's.
  "PlayOrDraw" -> Just . R.Text $ Text.pack "Play"
  "ChooseAction" -> R.Number <$> Offer.onlyPass (Question.offer question)
  "ChooseManaToSpend" | Just (unit : rest) <- Protocol.arrayOf offer, all (== unit) rest -> Just unit
  "AssignCombatDamage"
    | Just [recipient] <- Protocol.field "lethal" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf,
      Just total <- Protocol.field "total" offer >>= Protocol.naturalOf ->
        Just $ R.Array [R.Array [fst recipient, R.Number $ toInteger total] | total > 0]
  _ -> Nothing
  where
    question = Request.question request
    offer = Question.offer question

-- | Where a ChooseAction offer's pass sits.
passIndex :: Request.Request -> Maybe Integer
passIndex request = do
  Monad.guard $ Question.kind (Request.question request) == Text.pack "ChooseAction"
  options' <- Protocol.arrayOf (Question.offer (Request.question request))
  fmap toInteger (List.findIndex isPass options')

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
    logged :: Seq.Seq Text.Text,
    -- | How many of the newest entries arrived since the seat last decided.
    fresh :: Natural,
    -- | The blocker an attacker's tile assigns to when tapped (MkFocus).
    focus :: Maybe Natural
  }
  deriving (Eq, Show)

-- | A screen with nothing picked, read or logged.
blank :: Screen
blank = MkScreen Set.empty Nothing Seq.empty 0 Nothing

-- | The page for a request: the turn bar, the table with each tile marked
-- for what tapping it does, the question docked with its answers, the log,
-- and the sheet of the card being read.
view :: Request.Request -> Screen -> Html.Html Event
view request screen =
  let marks = cues request (selected screen) (focus screen)
      board = Request.view request
   in Element.el "main" ["game"] [] $
        foldMap (\b -> [Board.turnBar b]) board
          <> [Element.el "div" ["play-area"] [] (foldMap (\b -> [Board.view marks b]) board)]
          <> foldMap (Board.hand marks) board
          <> [ Element.el "aside" ["side"] [] $
                 [ Element.el "section" ["prompt"] [Element.attr "aria-live" (Text.pack "polite")] $
                     [Element.el "p" ["problem"] [Element.attr "role" (Text.pack "alert")] [Html.text problem] | problem <- Maybe.maybeToList (Request.problem request)]
                       <> news screen
                       <> form request (selected screen)
                 ]
                   <> [logView screen | not (Seq.null (logged screen))]
             ]
          <> Maybe.maybeToList (do b <- board; r <- reading screen; sheetFor request b marks r)

-- | What happened since the seat last decided, the latest few, oldest
-- first; the log holds the rest.
news :: Screen -> [Html.Html msg]
news screen =
  let count = Natural.toIntSaturating (fresh screen)
      shown = min 4 count
      latest = Seq.drop (Seq.length (logged screen) - shown) (logged screen)
   in [ Element.el "ul" ["news"] [Element.attr "aria-label" (Text.pack "Since your last decision")] $
          [Element.el "li" ["more"] [] [Element.string ("\x2026and " <> show (count - shown) <> " more in the log")] | count > shown]
            <> [Element.el "li" [] [] (Mana.rich t) | t <- Foldable.toList latest]
      | shown > 0
      ]

-- | The log, newest first, the entries since the seat last decided lit; its
-- summary the newest entry.
logView :: Screen -> Html.Html msg
logView screen =
  let entries = Seq.reverse (Seq.mapWithIndex (,) (logged screen))
      newest = Natural.toIntSaturating (fresh screen)
      total = Seq.length (logged screen)
   in Element.el
        "details"
        ["log"]
        []
        [ Element.el "summary" [] [] [Element.el "span" ["k"] [] [Element.string "Log"], Element.el "span" ["latest"] [] (foldMap (\(_, t) -> [Element.text t]) (Seq.lookup 0 entries))],
          Element.keyed "ol" ["log-list"] [Element.attr "reversed" Text.empty] (Foldable.toList (fmap (\(i, t) -> (Text.pack (show i), Element.el "li" ["fresh" | i >= total - newest] [] [Element.text t])) (Seq.take 300 entries)))
        ]

-- | The sheet for the card being read, with what the seat may do with it
-- now: its actions, or picking it.
sheetFor :: Request.Request -> PlayerView.PlayerView -> Board.Marks Event -> Card.Reading -> Maybe (Html.Html Event)
sheetFor request board marks r = do
  o <- Map.lookup (Card.object r) (Render.objects board)
  let cue = Board.object marks o
      actions = case Card.tap cue of
        MkRead _ -> [primary (MkAnswer (R.Number i)) label | (i, label) <- Map.findWithDefault [] (Card.object r) (actionsByObject request)]
        MkToggle g i -> [primary (MkToggle g i) (Text.pack (if elem "chosen" (Card.marks cue) then "Unselect" else "Select"))]
        MkAnswer reply -> [primary (MkAnswer reply) (Text.pack "Use")]
        MkFocus g -> [primary (MkFocus g) (Text.pack "Block with this")]
        _ -> []
  pure (Card.sheet board o r actions MkClose)

-- | Each object's actions in a ChooseAction offer, by index and label;
-- every copy of a card in hand its own, though the prompt lists one
-- (Offer.distinct).
actionsByObject :: Request.Request -> Map.Map ObjectId.ObjectId [(Integer, Text.Text)]
actionsByObject request = case (Text.unpack (Question.kind (Request.question request)), Request.view request, Protocol.arrayOf (Question.offer (Request.question request))) of
  ("ChooseAction", Just shown, Just options') ->
    Map.fromListWith (flip (<>)) [(oid, [(i, Offer.actionName shown option)]) | (i, option) <- zip [0 ..] options', Just oid <- [Protocol.field "object" option >>= Offer.objectOf]]
  _ -> Map.empty

-- | How each tile and seat looks and what tapping it does for this request:
-- a card the seat may act with is lit and opens its sheet; a payment's
-- sources pay when tapped; an option of a picker toggles when tapped, where
-- it is offered once; every other tile opens its sheet.
cues :: Request.Request -> Set.Set (Natural, Natural) -> Maybe Natural -> Board.Marks Event
cues request chosen focused =
  let question = Request.question request
      offer = Question.offer question
      acting = actionsByObject request
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
      toggled (g, i) = Card.MkCue [if Set.member (g, i) chosen then "chosen" else "choosable"] (MkToggle g i)
      -- Blocks are made on the table: a blocker's tile, then the tile of the
      -- attacker it blocks.
      (blockers, attackers) = Maybe.fromMaybe ([], []) (blocks request)
      blockerCue oid = do
        g <- List.elemIndex oid blockers
        let gn = Int.toNaturalSaturating g
            assigned = any ((== gn) . fst) (Set.toList chosen)
        pure (Card.MkCue [if focused == Just gn then "focused" else "choosable"] (MkFocus gn) `withMark` ["blocking" | assigned])
      attackerCue oid = do
        i <- Int.toNaturalSaturating <$> List.elemIndex oid attackers
        pure $ case focused of
          Just g -> Card.MkCue ["attacking", if Set.member (g, i) chosen then "chosen" else "choosable"] (MkToggle g i)
          Nothing -> Card.MkCue ["attacking"] (MkRead oid)
      objectCue o =
        let oid = ObjectView.object o
         in case (blockerCue oid, attackerCue oid, Map.lookup oid objects, Map.lookup oid sources) of
              (Just cue, _, _, _) -> cue
              (_, Just cue, _, _) -> cue
              (_, _, Just gi, _) -> toggled gi
              (_, _, _, Just source) -> Card.MkCue ["choosable"] (MkAnswer source)
              _ -> Card.MkCue ["playable" | Map.member oid acting] (MkRead oid)
   in Board.MkMarks objectCue (\pid -> fmap toggled (Map.lookup pid players))

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

-- | Before a seat's question when the screen last showed another seat's.
handOff :: PlayerId.PlayerId -> Html.Html Event
handOff seat =
  Element.el
    "main"
    ["handoff"]
    []
    [ Element.el "p" [] [] [Html.text $ Text.pack "Pass the screen to " <> Render.player seat <> Text.pack "."],
      primary MkReady $ Text.pack "Ready"
    ]

-- | What an event does, the options picked so far being `picked`.
step :: Request.Request -> Set.Set (Natural, Natural) -> Event -> Text.Text -> Outcome
step request picked event typed = case event of
  MkAnswer reply -> MkReplied reply
  MkToggle g i
    | Just (_, items) <- ordering request ->
        let next = if i `elem` fmap snd (Set.toList picked) then picked else Set.insert (Natural.length (Set.toList picked), i) picked
         in if Natural.length (Set.toList next) == Natural.length items then MkReplied . R.Array . fmap (R.Number . toInteger . snd) $ Set.toAscList next else MkPicking next
    | Just (_, from) <- piles request ->
        let placed = Set.toAscList picked
            next = if i `elem` fmap snd placed then picked else Set.insert (Natural.length placed * 2 + g, i) picked
            pile p = R.Array [List.genericIndex from j | (k, j) <- Set.toAscList next, k `mod` 2 == p]
         in if Natural.length (Set.toList next) == Natural.length from then MkReplied $ R.Array [pile 1, pile 0] else MkPicking next
    | Set.member (g, i) picked -> MkPicking $ Set.delete (g, i) picked
    | Just True <- exclusive <$> (picker request >>= \chooser -> Maybe.listToMaybe (List.genericDrop g (groups chooser))) -> MkPicking . Set.insert (g, i) $ Set.filter ((/= g) . fst) picked
    | otherwise -> MkPicking $ Set.insert (g, i) picked
  MkSubmit -> case picker request of
    Just chooser ->
      let chosen = [[reply | (i, (reply, _)) <- zip [0 ..] (options group), Set.member (g, i) picked] | (g, group) <- zip [0 ..] (groups chooser)]
          wrong = [group | (group, picks) <- zip (groups chooser) chosen, Natural.length picks < least group || Natural.length picks > most group]
       in case wrong of
            group : _ -> MkRefused $ title group <> Text.pack (": pick " <> bounds group <> ".")
            [] -> either MkRefused MkReplied $ finish chooser chosen
    Nothing -> MkPicking picked
  MkTyped
    | Question.kind (Request.question request) == Text.pack "ChooseCardName" ->
        if Text.null (Text.strip typed) then MkRefused (Text.pack "Type the name of a card.") else MkReplied . R.Text $ Text.strip typed
  MkTyped -> either (MkRefused . (Text.pack "That reply does not read: " <>)) MkReplied $ Common.parse typed >>= Codec.Reply.fromValue
  MkReady -> MkPicking picked
  MkClear -> MkPicking Set.empty
  MkRead _ -> MkPicking picked
  MkClose -> MkPicking picked
  MkFocus _ -> MkPicking picked
  MkSkip -> maybe (MkRefused $ Text.pack "There is no pass to make.") (MkReplied . R.Number) $ passIndex request
  MkAuto -> maybe (MkRefused $ Text.pack "Auto can't pay the rest of this cost. Choose a source.") MkReplied $ auto request

bounds :: Group -> String
bounds group
  | least group == most group = show (least group)
  | otherwise = "from " <> show (least group) <> " to " <> show (most group)

-- | The groups a question is answered by picking among, for the questions
-- that are: cards to put on the bottom or discard (CR 103.5, 701.9),
-- attackers (CR 508.1a), blocks (CR 509.1a), targets (CR 601.2c), cards
-- found by a search (CR 701.23b), a set from an offer's "among" that picks no
-- option twice, mana to spend, and combat damage divided (CR 510.1c).
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
      pure $ MkPicker [MkGroup (Text.pack "Choose " <> Render.count wanted "card" <> Text.pack purpose) (named from) wanted wanted False] (Right . R.Array . concat) (Text.pack "Done")
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
             in MkGroup (Text.pack "Choose " <> Render.count n "target" <> Text.pack " for " <> Offer.objectName shown source <> foldMap (\t -> Text.pack " (" <> Render.spaced t <> Text.pack ")") (Offer.textOf slot)) [(option, Offer.targetName shown legal option) | option <- legal] n n False
          | (slot, wanted, offered) <- slots
          ]
          (\chosen -> Right $ R.Array [R.Array [slot, R.Array picks] | ((slot, _, _), picks) <- zip slots chosen])
          (Text.pack "Done")
    "Search" -> do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      names <- Protocol.field "names" offer >>= Protocol.arrayOf >>= traverse Offer.textOf
      wanted <- min (Natural.length from) <$> (Protocol.field "count" offer >>= Protocol.naturalOf)
      pure $ MkPicker [MkGroup (Text.pack "Choose up to " <> Render.count wanted "card" <> Text.pack " to find") (zip from names) 0 wanted False] (Right . R.Array . concat) (Text.pack "Find")
    _
      | Just among <- Protocol.field "among" offer >>= Protocol.arrayOf,
        Protocol.field "repeats" offer /= Just (R.Boolean True) -> do
          let least' = Maybe.fromMaybe 0 $ Protocol.field "least" offer >>= Protocol.naturalOf
              most' = Maybe.fromMaybe (Natural.length among) $ Protocol.field "most" offer >>= Protocol.naturalOf
          pure $ MkPicker [MkGroup (Render.spaced (Question.kind question) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer)) [(option, Offer.optionName shown option) | option <- among] least' most' False] (Right . R.Array . concat) (Text.pack "Done")
    "ChooseManaToSpend" -> do
      units <- Protocol.arrayOf offer
      shownUnits <- traverse (Offer.decoded Codec.ManaUnit.codec) units
      pure $ MkPicker [MkGroup (Text.pack "Choose the mana to spend") (zip units $ fmap Offer.unitName shownUnits) 1 1 True] (Right . Maybe.fromMaybe R.Null . Maybe.listToMaybe . concat) (Text.pack "Spend")
    "AssignCombatDamage" -> do
      (source, lethal, total) <- damage
      let amounts = [(R.Number $ toInteger n, Text.pack $ show n) | n <- [0 .. total]]
      pure $
        MkPicker
          [MkGroup (Offer.recipientName shown recipient <> (if threshold > 0 then Text.pack (" (lethal " <> show threshold <> ")") else Text.empty)) amounts 1 1 True | (recipient, threshold) <- lethal]
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

-- | What a question orders, first to last, answered as a permutation of its
-- items' indices (Parun.Engine.Game.permute): triggered abilities put on the
-- stack at once (CR 603.3b), and any offer listing an "order".
ordering :: Request.Request -> Maybe (Text.Text, [Text.Text])
ordering request = do
  shown <- Request.view request
  if Question.kind question == Text.pack "OrderTriggers"
    then (,) (Text.pack "Order your triggered abilities, first to last. The last resolves first.") . fmap (Offer.triggerName shown) <$> Protocol.arrayOf offer
    else do
      items <- Protocol.field "order" offer >>= Protocol.arrayOf
      pure (capitalized (Question.kind question) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer) <> Text.pack ", first to last: " <> Offer.orderNote (Question.kind question) offer <> Text.pack ".", fmap (Offer.optionName shown) items)
  where
    question = Request.question request
    offer = Question.offer question

-- | A scry, surveil or fateseal: its heading, the other pile's name, and the
-- cards, each placed on top or in the other pile in the order tapped, sent
-- as (other, top) (CR 701.22a, 701.25a, 701.29a).
piles :: Request.Request -> Maybe ((Text.Text, Text.Text), [R.Reply])
piles request = do
  words' <- case Text.unpack (Question.kind question) of
    "ChooseScry" -> Just ("Scry", "Bottom")
    "ChooseSurveil" -> Just ("Surveil", "Graveyard")
    "ChooseFateseal" -> Just ("Fateseal", "Bottom")
    _ -> Nothing
  from <- Protocol.field "from" (Question.offer question) >>= Protocol.arrayOf
  pure (Bifunctor.bimap Text.pack Text.pack words', from)
  where
    question = Request.question request

-- | A kind's words, the first capitalized: ArrangeLibraryCards as "Arrange library cards".
capitalized :: Text.Text -> Text.Text
capitalized kind = case Text.uncons (Render.spaced kind) of
  Just (c, rest) -> Text.cons (Char.toUpper c) rest
  Nothing -> Text.empty

form :: Request.Request -> Set.Set (Natural, Natural) -> [Html.Html Event]
form request picked = case Text.unpack (Question.kind question) of
  "PlayOrDraw" -> heading "Play first or draw?" : [button (MkAnswer option) (Offer.tagName word) | option@(R.Text word) <- Foldable.fold (Protocol.arrayOf offer)]
  "DeclareMulligan" ->
    heading ("Keep this hand, or mulligan?" <> foldMap (\n -> " A mulligan puts " <> Text.unpack (Render.count n "card") <> " on the bottom.") (Protocol.field "bottom" offer >>= Protocol.naturalOf))
      : [primary (MkAnswer . R.Text $ Text.pack "Keep") (Text.pack "Keep"), button (MkAnswer . R.Text $ Text.pack "Mulligan") (Text.pack "Mulligan")]
  "ChooseManaToSpend"
    | Just units <- Protocol.arrayOf offer,
      Just shownUnits <- traverse (Offer.decoded Codec.ManaUnit.codec) units ->
        heading "Choose the mana to spend." : [button (MkAnswer unit) (Offer.unitName shown) | (unit, shown) <- zip units shownUnits]
  "ChooseAction"
    | Just shown <- Request.view request,
      Just options' <- Protocol.arrayOf offer ->
        let (passes, rest) = List.partition (isPass . snd) (Offer.distinct shown options')
            yours = PlayerView.active shown == PlayerView.viewer shown
            settled = null (PlayerView.stack shown)
            prompt
              | not settled = "Respond, or pass to let the top of the stack resolve."
              | yours = "Your turn. Play something, or pass to move on."
              | otherwise = "Their turn. Respond, or pass."
         in heading prompt
              : [Element.el "div" ["actions"] [] [button (MkAnswer $ R.Number i) (Offer.actionName shown option) | (i, option) <- rest] | not (null rest)]
                <> [ Element.el "div" ["passes"] [] $
                       [primary (MkAnswer $ R.Number i) (Text.pack (if settled then "Pass" else "Pass and resolve")) | (i, _) <- passes]
                         <> [button MkSkip (Text.pack (if yours then "End turn" else "Pass to my turn")) | settled, not (null passes)]
                   ]
  kind
    | kind `elem` ["ChooseManaSource", "ChooseExtraManaSource"],
      Just shown <- Request.view request,
      Just sources <- Protocol.field "sources" offer >>= Protocol.arrayOf >>= traverse Offer.manaSource ->
        Html.element (Text.pack "h2") [] [Html.text $ foldMap (\cost -> Text.pack "Paying " <> Render.manaCost cost <> Text.pack ". ") (Protocol.field "cost" offer >>= Offer.decoded Codec.ManaCost.codec) <> Text.pack (if kind == "ChooseManaSource" then "Choose a source." else "The cost is covered. Activate another source, or pay now.")]
          : [primary MkAuto (Text.pack "Pay automatically") | Maybe.isJust (auto request)]
            <> [button (MkAnswer source) (Offer.listedName shown source <> makes yields) | (source, yields) <- sources]
            <> [(if kind == "ChooseManaSource" then button else primary) (MkAnswer R.Null) $ Text.pack (if kind == "ChooseManaSource" then "Stop paying" else "Pay now")]
  kind
    | kind `elem` ["ChooseOptional", "ReverseManaAbilities"],
      Just shown <- Request.view request ->
        heading
          ( Text.unpack $
              if kind == "ChooseOptional"
                then foldMap (Offer.objectName shown) (Protocol.field "source" offer) <> Text.pack ": use its optional effect?"
                else Text.pack "That action was reversed. Reverse the mana ability of " <> Text.intercalate (Text.pack " and ") (foldMap (fmap (Offer.listedName shown)) (Protocol.field "sources" offer >>= Protocol.arrayOf)) <> Text.pack " too?"
          )
          : [button (MkAnswer . R.Text $ Text.pack tag) (Text.pack label) | (tag, label) <- [("Exercises", "Yes"), ("Declines", "No")]]
  kind
    | Just shown <- Request.view request,
      Nothing <- Protocol.field "among" offer,
      Nothing <- Protocol.field "amounts" offer,
      Just least' <- Protocol.field "least" offer >>= Protocol.naturalOf,
      Just greatest <- Protocol.field "greatest" offer >>= Protocol.naturalOf ->
        heading (Text.unpack ((if kind == "ChooseX" then Text.pack "Choose " <> Foldable.fold (Protocol.field "variable" offer >>= Offer.textOf) else capitalized (Question.kind question)) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer)) <> " (" <> show least' <> " to " <> show greatest <> ").")
          : if greatest - least' <= 20
            then [button (MkAnswer . R.Number $ toInteger n) (Text.pack $ show n) | n <- [least' .. greatest]]
            else [Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "A number"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []]
  _
    | Just (title', items) <- ordering request ->
        Html.element (Text.pack "h2") [] [Html.text title']
          : [button (MkToggle 0 i) $ foldMap (\(k, _) -> Text.pack (show (k + 1) <> ". ")) (List.find ((== i) . snd) (Set.toList picked)) <> label | (i, label) <- zip [0 ..] items]
            <> [button (MkAnswer . R.Array $ fmap (R.Number . toInteger) [0 .. length items - 1]) (Text.pack "Keep this order"), button MkClear (Text.pack "Start over")]
  _
    | Just ((what, other), from) <- piles request,
      Just shown <- Request.view request ->
        let placed = Set.toAscList picked
            at j = List.find ((== j) . snd) placed
         in Html.element (Text.pack "h2") [] [Html.text $ what <> Text.pack ": tap each card for the top or the " <> Text.toLower other <> Text.pack " pile, in order. The first tapped for the top stays on top."]
              : concat
                [ case at j of
                    Just (k, _) -> [Html.element (Text.pack "p") [] [Html.text $ Text.pack (show (k `div` 2 + 1) <> ". ") <> (if even k then Text.pack "Top: " else other <> Text.pack ": ") <> Offer.listedName shown card]]
                    Nothing -> [button (MkToggle 0 j) (Text.pack "Top: " <> Offer.listedName shown card), button (MkToggle 1 j) (other <> Text.pack ": " <> Offer.listedName shown card)]
                | (j, card) <- zip [0 ..] from
                ]
                <> [button (MkAnswer $ R.Array [R.Array [], R.Array from]) (Text.pack "Keep all on top"), button MkClear (Text.pack "Start over")]
  "ChooseCardName" -> [heading "Name a card.", Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "A card's name"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []]
  _
    | Just shown <- Request.view request,
      Just (options', name) <- Monad.mplus (fmap (\o -> (o, Offer.optionName shown)) (Protocol.field "options" offer >>= Protocol.arrayOf)) (fmap (\o -> (o, Offer.playerName)) (Protocol.field "players" offer >>= Protocol.arrayOf)) ->
        let entries = foldMap (fmap Just) (Protocol.field "entries" offer >>= Protocol.arrayOf) <> repeat Nothing
         in Html.element (Text.pack "h2") [] [Html.text $ capitalized (Question.kind question) <> foldMap (\source -> Text.pack " for " <> Offer.objectName shown source) (Protocol.field "source" offer) <> Text.pack "."]
              : [button (MkAnswer option) (maybe (name option) (Offer.entryName shown) entry) | (option, entry) <- zip options' entries]
  "DeclareAttackers"
    | Just attackers <- Protocol.arrayOf offer,
      Just shown <- Request.view request ->
        let count = Set.size picked
         in [ heading "Declare attackers: tap your creatures to attack with them.",
              Element.el "div" ["actions"] [] [toggle 0 i (Offer.listedName shown option) | (i, option) <- zip [0 ..] attackers],
              Element.el "div" ["passes"] [] $
                [primary MkSubmit (Text.pack (if count == 0 then "No attack" else "Attack with " <> show count))]
                  <> [button (MkAnswer (R.Array attackers)) (Text.pack "All") | length attackers > 1, count < length attackers]
            ]
  "DeclareBlockers"
    | Just chooser <- picker request ->
        heading "Declare blockers: tap a creature of yours, then the attacker it blocks."
          : concat
            [ Element.el "h3" ["subhead"] [] [Html.text (title group)]
                : [toggle g i label | (i, (_, label)) <- zip [0 ..] (options group)]
            | (g, group) <- zip [0 ..] (groups chooser)
            ]
            <> [primary MkSubmit (Text.pack (if Set.null picked then "No blocks" else "Block"))]
  _
    | Just chooser <- picker request ->
        concat
          [ Html.element (Text.pack "h2") [] [Html.text $ title group <> Text.pack (" (" <> bounds group <> ")")]
              : [toggle g i label | (i, (_, label)) <- zip [0 ..] (options group)]
          | (g, group) <- zip [0 ..] (groups chooser)
          ]
          <> [primary MkSubmit $ send chooser]
  _ ->
    [ heading (Text.unpack (Render.spaced (Question.kind question)) <> ":"),
      Html.element (Text.pack "pre") [] [Html.text . Common.render $ Codec.Reply.toValue offer],
      Html.element (Text.pack "input") [Attribute.MkProperty (Text.pack "placeholder") (Text.pack "Reply as JSON"), Attribute.MkProperty (Text.pack "data-clear") Text.empty, Attribute.MkOn (Text.pack "change") MkTyped] []
    ]
  where
    question = Request.question request
    offer = Question.offer question
    toggle g i label = button (MkToggle g i) $ (if Set.member (g, i) picked then Text.pack "\x2713 " else Text.empty) <> label
    makes yields = if null yields then Text.empty else Text.pack " (makes " <> Text.intercalate (Text.pack " or ") (fmap (foldMap Render.manaType) yields) <> Text.pack ")"
    heading t = Html.element (Text.pack "h2") [] [Html.text $ Text.pack t]

button :: Event -> Text.Text -> Html.Html Event
button event label = Element.button [] event (Mana.rich label)

-- | The button that answers, set apart from the rest.
primary :: Event -> Text.Text -> Html.Html Event
primary event label = Element.button ["primary"] event (Mana.rich label)
