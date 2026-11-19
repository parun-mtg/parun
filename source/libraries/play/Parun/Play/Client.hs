-- | The text client: a person answering request lines at a terminal. It reads
-- only the Offer.decoded line and the card data, never the game, so anything it
-- cannot show is a gap in the protocol.
module Parun.Play.Client where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Codec.CombatDamageOption as Codec.CombatDamageOption
import qualified Parun.Codec.CostComponent as Codec.CostComponent
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.ManaCost as Codec.ManaCost
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.ManaUnit as Codec.ManaUnit
import qualified Parun.Codec.NumberFloor as Codec.NumberFloor
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Engine.PlayerView as PlayerView.Engine
import qualified Parun.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Oracle.Cost as Oracle.Cost
import qualified Parun.Oracle.Keyword as Oracle.Keyword
import qualified Parun.Play.Offer as Offer
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Types.AbilityFacts as AbilityFacts
import qualified Parun.Types.AbilityKind as AbilityKind
import qualified Parun.Types.AbilityWasActivated as AbilityWasActivated
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CombatDamageOption as CombatDamageOption
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Hotseat as Hotseat
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaUnit as ManaUnit
import qualified Parun.Types.NumberFloor as NumberFloor
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Terminal as Terminal
import qualified Parun.Types.Undoing as Undoing
import qualified Text.Read as Read

-- | The client's monad: what it keeps between lines, and why play stopped.
type Client m = State.StateT Hotseat.Hotseat (Except.ExceptT Text.Text m)

-- | Answers one request line with a reply line, asking the person at the
-- terminal. A seat change first clears the screen and waits for the next
-- player. Fails with why play stopped: the end of input, or "quit".
--
-- CR 104.3a: a seat concedes by typing "concede" at any question, and the
-- client answers the engine's next Concede prompt for that seat with it. A
-- card's printed text comes from the card data, by a name the view shows, and
-- card data that cannot be read stops play, saying why.
respond :: (Monad m) => Terminal.Terminal m -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> Text.Text -> Client m Text.Text
respond terminal lookUp line = do
  request <- either (stop . (Text.pack "unreadable request: " <>)) pure (Common.parse line >>= Codec.decode Codec.Request.codec)
  let question = Request.question request
      seat = Question.seat question
  -- The hotseat asks CR 103.1's play or draw at the terminal itself
  -- (Parun.Play.Game.playOrDraw), so every request it reads has a view.
  view <- maybe (stop (Text.pack "a request with no view")) pure (Request.view request)
  collect view
  Monad.when (Question.kind question == Text.pack "ChooseAction") (undone seat view)
  Monad.when (Question.kind question `elem` fmap Text.pack ["DeclareAttackers", "DeclareBlockers"]) (redeclaring seat question view)
  fmap (Common.render . Codec.Reply.toValue) $
    if Question.kind question == Text.pack "Concede"
      then do
        conceding <- State.gets (Set.member seat . Hotseat.conceding)
        pure (R.Text (Text.pack (if conceding then "Concedes" else "Continues")))
      else do
        (said, automatic) <- unasked view request
        case automatic of
          Nothing -> do
            mapM_ (say terminal) said
            reply <- asked terminal lookUp view request
            attempt seat view question reply
            pure reply
          Just reply -> do
            -- Only the seat the screen faces is shown its news unasked.
            facing <- State.gets Hotseat.facing
            news <- if facing == Just seat then tell False seat else pure []
            mapM_ (say terminal) (news <> said)
            pure reply

-- | Remembers a cast or an activation the seat chose, so its next priority
-- can tell it whether the action was undone.
attempt :: (Monad m) => PlayerId.PlayerId -> PlayerView.PlayerView -> Question.Question -> R.Reply -> Client m ()
attempt seat view question reply = case reply of
  R.Number i
    | Question.kind question == Text.pack "ChooseAction",
      Just option <- Protocol.arrayOf (Question.offer question) >>= Maybe.listToMaybe . List.genericDrop i,
      Just kind <- Protocol.field "action" option >>= Offer.textOf,
      kind `elem` [Text.pack "Cast", Text.pack "Activate"] ->
        let mark = (PlayerView.game view, PlayerView.turn view, Natural.length (PlayerView.witnessed view))
         in State.modify' (\h -> h {Hotseat.attempted = Map.insert seat (mark, Offer.actionName view option) (Hotseat.attempted h)})
  _ -> pure ()

-- | CR 734.1: at the seat's priority after a cast or an activation it chose,
-- a line of news saying it was undone when its view has logged no spell cast
-- and no non-mana ability activated since, and why where the view says
-- (PlayerView.undone).
undone :: (Monad m) => PlayerId.PlayerId -> PlayerView.PlayerView -> Client m ()
undone seat view = do
  tried <- State.gets (Map.lookup seat . Hotseat.attempted)
  State.modify' (\h -> h {Hotseat.attempted = Map.delete seat (Hotseat.attempted h)})
  case tried of
    Just ((game, turn, n), action)
      | game == PlayerView.game view,
        turn == PlayerView.turn view,
        not (any (completes . LoggedEvent.event) (List.genericDrop n (PlayerView.witnessed view))) ->
          tellUndone seat action (PlayerView.undone view)
    _ -> pure ()
  where
    completes e = case e of
      GameEvent.SpellCast {} -> True
      -- CR 734.1: a mana ability paid toward an undone action and kept is
      -- logged too, and is not the action completing.
      GameEvent.AbilityActivated a -> AbilityFacts.kind (AbilityWasActivated.facts a) /= AbilityKind.ManaAbility
      _ -> False

-- | CR 734.1 with CR 508.1 / 509.1's preambles: at a declaration of attackers
-- or blockers, a line of news saying the seat's last one was undone when its
-- view says why (PlayerView.undone), which the engine notes only for a
-- declaration it asks again.
redeclaring :: (Monad m) => PlayerId.PlayerId -> Question.Question -> PlayerView.PlayerView -> Client m ()
redeclaring seat question view = Monad.forM_ (PlayerView.undone view) $ \why ->
  tellUndone seat (Text.pack (if Question.kind question == Text.pack "DeclareAttackers" then "The declaration of attackers" else "The declaration of blockers")) (Just why)

-- | The line of news saying `action` was undone, and why where the view says.
tellUndone :: (Monad m) => PlayerId.PlayerId -> Text.Text -> Maybe Undoing.Undoing -> Client m ()
tellUndone seat action why =
  let line = Text.pack "Undone: " <> action <> Text.pack " could not be completed" <> foldMap because why <> Text.pack ", so it was reversed and its payments canceled."
   in State.modify' (\h -> h {Hotseat.news = Map.insertWith (flip (<>)) seat (Seq.singleton line) (Hotseat.news h)})
  where
    because reason = Text.pack $ case reason of
      Undoing.IllegalProposal -> " (a choice announced for it was illegal)"
      Undoing.UnpaidCost -> " (its cost was not paid)"
      Undoing.IllegalDeclaration -> " (it broke a restriction or requirement on attacking or blocking)"
      Undoing.RepeatedLoop -> " (it would repeat a loop, so a different choice is needed)"

-- | Asks the seat, handing the screen to it first.
asked :: (Monad m) => Terminal.Terminal m -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerView.PlayerView -> Request.Request -> Client m R.Reply
asked terminal lookUp view request = do
  let question = Request.question request
      seat = Question.seat question
  handed <- handOff terminal seat
  first <- State.gets (Set.notMember seat . Hotseat.greeted)
  State.modify' (\h -> h {Hotseat.greeted = Set.insert seat (Hotseat.greeted h)})
  news <- tell handed seat
  mapM_ (say terminal) $
    [Text.empty]
      <> Render.board (Question.kind question `notElem` fmap Text.pack beforeFirstTurn) view
      <> news
      <> Render.hand view
      <> [Text.pack "Type card and a number from a list, or a card's name, to read that card." | first]
      <> foldMap (\p -> [Text.pack "That answer was refused: " <> p]) (Request.problem request)
  answer terminal lookUp seat view question

-- | CR 103.5-103.6: the questions asked before the first turn (CR 103.8).
beforeFirstTurn :: [String]
beforeFirstTurn = ["KeepHand", "DeclareMulligan", "Bottom", "MulliganAction", "OpeningHandAction"]

-- | CR 104.3a's concession, quitting and reading a card are typed at any
-- question, and "auto" or an empty line at a mana source; everything else is
-- read by the question's own reader until it reads.
answer :: (Monad m) => Terminal.Terminal m -> (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerId.PlayerId -> PlayerView.PlayerView -> Question.Question -> Client m R.Reply
answer terminal lookUp seat view question = case (Question.kind question, Offer.slotsOf question) of
  (kind, Just (source, slots)) | kind == Text.pack "ChooseTargets" -> fmap R.Array (mapM (target source (length slots > 1)) slots)
  _ -> ask (listedObjects view (Question.offer question)) (choice view question)
  where
    ask listed (shown, reader) = do
      mapM_ (say terminal) shown
      let loop = do
            typed <- fmap Text.strip (listen terminal)
            case Text.unpack typed of
              "quit" -> stop (Text.pack "quit")
              "concede" -> do
                State.modify' (\h -> h {Hotseat.conceding = Set.insert seat (Hotseat.conceding h)})
                say terminal (Text.pack "You concede at your next priority. Answer this question first.")
                loop
              _
                | Just wanted <- Text.stripPrefix (Text.pack "card") typed,
                  maybe True (Char.isSpace . fst) (Text.uncons wanted) -> do
                    either (stop . Text.pack . Exception.displayException) (mapM_ (say terminal)) (reading lookUp view listed (Text.strip wanted))
                    loop
              -- The rest of this payment, by Offer.autoSource.
              word
                | word `elem` ["auto", ""],
                  Question.kind question == Text.pack "ChooseManaSource" ->
                    autoPay seat view (Question.offer question) >>= \paid -> case paid of
                      Just (said, reply) -> say terminal said >> pure reply
                      Nothing -> refuse (Text.pack "Auto can't pay the rest of this cost. Choose a source.")
              _ -> case reader typed of
                Right reply -> pure reply
                Left problem -> do
                  -- A card's name the answer does not read shows that card.
                  found <- either (stop . Text.pack . Exception.displayException) pure (readingByName lookUp view typed)
                  if null found then refuse problem else mapM_ (say terminal) found >> loop
          refuse problem = mapM_ (say terminal) (problem : shown) >> loop
      loop
    -- A slot is named only to tell it from the spell's or ability's others.
    target source several (slot, wanted, offered) = do
      let legal = Offer.bySeat view offered
          n = min wanted (Natural.length legal)
          named = if several then foldMap (\t -> Text.pack " (" <> Render.spaced t <> Text.pack ")") (Offer.textOf slot) else Text.empty
          shown =
            Text.concat [Text.pack "Choose ", Render.count n "target", Text.pack " for ", Offer.objectName view source, named, Text.pack ":"]
              : numbered (fmap (Offer.targetName view legal) legal)
      picked <- ask (fmap Offer.recipientObject legal) (shown, fmap R.Array . exactly n legal)
      pure (R.Array [slot, picked])

-- | What a question shows, and how a typed line is read into its answer.
choice :: PlayerView.PlayerView -> Question.Question -> ([Text.Text], Text.Text -> Either Text.Text R.Reply)
choice view question = Maybe.fromMaybe generic $ case Text.unpack (Question.kind question) of
  "ChooseAction" -> do
    shown <- fmap (Offer.distinct view) (Protocol.arrayOf offer)
    let pass = List.find ((== Just (Text.pack "Pass")) . (Protocol.field "action" Monad.>=> Offer.textOf) . snd) shown
    pure
      ( Text.pack (if Maybe.isJust pass then "Choose an action, or press Enter to pass:" else "Choose an action:") : numbered (fmap (Offer.actionName view . snd) shown),
        \typed -> case pass of
          Just (i, _) | Text.null typed -> Right (R.Number i)
          _ -> fmap (\i -> R.Number (maybe 0 fst (Maybe.listToMaybe (List.genericDrop (i - 1) shown)))) (one (Natural.length shown) typed)
      )
  "DeclareMulligan" -> do
    bottom <- Protocol.field "bottom" offer >>= Protocol.naturalOf
    pure
      ( [ Text.pack "Keep this hand, or mulligan? A mulligan puts " <> Render.count bottom "card" <> Text.pack " on the bottom.",
          Text.pack "  1. Keep",
          Text.pack "  2. Mulligan"
        ],
        fmap (\i -> R.Text (Text.pack (if i == 1 then "Keep" else "Mulligan"))) . one 2
      )
  "Bottom" -> pickCount "to put on the bottom of your library"
  -- CR 404.3: every card of the batch, top first; Enter keeps the order shown.
  "ArrangeGraveyardArrivals" -> do
    from <- Protocol.field "from" offer >>= Protocol.arrayOf
    let size = Natural.length from
        wrong = Text.pack ("Type each number from 1 to " <> show size <> " once, or press Enter.")
    pure
      ( Text.pack "These cards go to your graveyard together. Type their numbers from the new top down, separated by spaces, or press Enter to keep this order (top first):" : numbered (fmap (Offer.listedName view) from),
        \typed -> case numbers typed of
          Right [] -> Right R.Null
          Right picked | List.sort picked == [1 .. size] -> Right (R.Array (fmap (R.Number . toInteger . subtract 1) picked))
          _ -> Left wrong
      )
  "ChooseDiscard" -> pickCount "to discard"
  "DeclareAttackers" -> do
    attackers <- Protocol.arrayOf offer
    pure
      ( Text.pack "Choose attackers by number or range such as 1-3, separated by spaces, all, or press Enter for none:" : numbered (fmap (Offer.listedName view) attackers),
        fmap R.Array . some False attackers
      )
  "DeclareBlockers" -> do
    blockers <- Protocol.field "blockers" offer >>= Protocol.arrayOf
    attackers <- Protocol.field "attackers" offer >>= Protocol.arrayOf
    pure
      ( [Text.pack "Attacking:"]
          <> numbered (fmap (Offer.listedName view) attackers)
          <> [Text.pack "Your creatures that can block:"]
          <> numbered (fmap (Offer.listedName view) blockers)
          <> [Text.pack "Type blocker:attacker pairs, such as 1:2, separated by spaces, or press Enter for no blocks:"],
        blocks blockers attackers
      )
  "ChooseManaSource" -> do
    cost <- Protocol.field "cost" offer >>= Offer.decoded Codec.ManaCost.codec
    let pool = foldMap (fmap ManaUnit.manaType . Mana.unwrap . SeatView.manaPool) (Offer.payerSeat view offer)
        held = foldMap (\t -> Text.pack "{" <> Render.manaType t <> Text.pack "}") pool
        left = foldMap (\(colored, rest) -> Text.pack ", leaving " <> Render.manaCost (ManaCost.MkManaCost ([ManaSymbol.Generic rest | rest > 0] <> fmap ManaSymbol.OfType colored))) (Offer.owing view offer)
        progress = if null pool then Text.empty else Text.pack " with " <> held <> Text.pack " in the pool" <> left
        substitutes = foldMap (Foldable.foldMap (Maybe.maybeToList . Maybe.listToMaybe . Oracle.Keyword.keywordWord Text.empty)) (Protocol.field "substitutes" offer >>= Offer.decoded (Common.list Codec.Keyword.codec))
        -- CR 601.2g-h: the substitution is asked once the mana window closes.
        later = if null substitutes then Text.empty else Text.pack ". After the mana, " <> Text.intercalate (Text.pack " or ") substitutes <> Text.pack " can pay part of it"
    sourcePick (Text.pack "Paying " <> Render.manaCost cost <> progress <> later <> Text.pack ". Choose a source to pay with, stop to stop paying, or auto (or press Enter) to pay the rest:") (Text.pack "stop") (Text.pack ", stop, or auto")
  -- CR 601.2g: mana abilities are activated before the cost is paid.
  "ChooseExtraManaSource" -> sourcePick (Text.pack "The cost is covered. Choose another source to activate, or press Enter to pay now:") Text.empty (Text.pack ", or press Enter to pay now")
  -- CR 734.1: whether the mana abilities of one activation are reversed too.
  "ReverseManaAbilities" -> do
    sources <- Protocol.field "sources" offer >>= Protocol.arrayOf
    pure
      ( [Text.pack "That action was reversed. Reverse the mana ability of " <> Text.intercalate (Text.pack " and ") (fmap (Offer.listedName view) sources) <> Text.pack " too?", Text.pack "  1. Yes", Text.pack "  2. No"],
        fmap (\i -> optionalDecision (if i == 1 then OptionalDecision.Exercises else OptionalDecision.Declines)) . one 2
      )
  -- CR 601.2b / 702.51a: each candidate as Oracle text prints its cost, and
  -- the keyword that offered it (CR 702.152a) and the delayed ability it
  -- creates (CR 603.7g); CR 101.4b: what earlier payers answered.
  "ChooseCost" -> do
    source <- Protocol.field "source" offer
    costs <- Protocol.field "costs" offer >>= Protocol.arrayOf
    let self = Offer.objectName view source
        keywords = foldMap (fmap (Offer.decoded Codec.Keyword.codec)) (Protocol.field "keywords" offer >>= Protocol.arrayOf) <> repeat Nothing
        arming = foldMap (fmap Offer.textOf) (Protocol.field "arming" offer >>= Protocol.arrayOf) <> repeat Nothing
        offered cost keyword arms = Offer.candidateName self cost <> foldMap (\k -> Text.pack " (" <> Render.spaced (Text.pack (Codec.Keyword.tagOf k)) <> Text.pack ")") keyword <> foldMap (\name -> Text.pack " (" <> name <> Text.pack ")") arms
    pure
      ( payersTold view self offer <> (Text.pack "Choose how to pay for " <> self <> Text.pack ":" : numbered (zipWith3 offered costs keywords arming)),
        fmap (R.Number . toInteger . subtract 1) . one (Natural.length costs)
      )
  -- CR 201.4: any card's name, typed in full; one the rules refuse is asked
  -- again, saying why (Parun.Play.Game.judgingNames).
  "ChooseCardName" ->
    pure
      ( [Text.pack "Choose a card name: type the name of a card."],
        \typed -> if Text.null typed then Left (Text.pack "Type the name of a card.") else Right (R.Text typed)
      )
  "ChooseManaYield" -> do
    source <- Protocol.field "source" offer
    yields <- Protocol.field "yields" offer >>= Offer.decoded (Common.list (Common.list Codec.ManaType.codec))
    pure
      ( Text.pack "Choose the mana " <> Offer.objectName view source <> Text.pack " makes:" : numbered (fmap (foldMap Render.manaType) yields),
        fmap (R.Number . toInteger . subtract 1) . one (Natural.length yields)
      )
  "ChooseManaSpentOnX" -> do
    source <- Protocol.field "source" offer
    shares <- Protocol.field "shares" offer >>= Offer.decoded (Common.list (Common.list Codec.ManaType.codec))
    pure
      ( Text.pack "Choose the mana you spent on X for " <> Offer.objectName view source <> Text.pack ":" : numbered (fmap (foldMap Render.manaType) shares),
        fmap (R.Number . toInteger . subtract 1) . one (Natural.length shares)
      )
  "ChooseX" -> do
    source <- Protocol.field "source" offer
    variable <- Protocol.field "variable" offer >>= Offer.textOf
    least <- Protocol.field "least" offer >>= Protocol.naturalOf
    greatest <- Protocol.field "greatest" offer >>= Protocol.naturalOf
    pure (ranged (Text.concat [Text.pack "Choose ", variable, Text.pack " for ", Offer.objectName view source]) least Text.empty (Just greatest))
  -- CR 608.2d: a bid's least is the high bid, which answering declines to top.
  "ChooseNumber" -> do
    source <- Protocol.field "source" offer
    least <- Protocol.field "least" offer >>= Protocol.naturalOf
    meaning <- Protocol.field "floor" offer >>= Offer.decoded Codec.NumberFloor.codec
    let note = if meaning == NumberFloor.HighBid then Text.pack ("; " <> show least <> " lets the high bid stand") else Text.empty
    pure (ranged (Text.pack "Choose a number for " <> Offer.objectName view source) least note (Protocol.field "greatest" offer >>= Protocol.naturalOf))
  -- CR 601.2b: how many times the keyword's cost is paid.
  "ChooseKicker" -> do
    source <- Protocol.field "source" offer
    least <- Protocol.field "least" offer >>= Protocol.naturalOf
    let keyword = Maybe.fromMaybe (Text.pack "its cost") (Protocol.field "keyword" offer >>= Offer.decoded Codec.Keyword.codec >>= Maybe.listToMaybe . Oracle.Keyword.printed (Offer.objectName view source))
    pure (ranged (Text.concat [Text.pack "Choose how many times to pay ", keyword, Text.pack " for ", Offer.objectName view source]) least Text.empty (Protocol.field "greatest" offer >>= Protocol.naturalOf))
  -- CR 702.132a: the generic mana the assisting player pays.
  "ChooseAssistAmount" -> bounded (Text.pack "Choose how much mana to pay to assist ")
  "ChoosePaidEnergy" -> bounded (Text.pack "Choose how much energy to pay for ")
  -- CR 715.3b.
  "ChooseReadAheadChapter" -> bounded (Text.pack "Choose the chapter to read ahead to for ")
  -- CR 706.2b: each die by its result, Enter declining.
  "RerollDice" -> do
    results <- Protocol.field "results" offer >>= Protocol.arrayOf
    let name option = case option of
          R.Number index
            | Just (R.Number result) <- Maybe.listToMaybe (List.genericDrop index results) -> Text.pack ("Die " <> show (index + 1) <> " showing " <> show result)
          _ -> Offer.optionName view option
    subset name Text.empty R.Array
  -- CR 706.2b: each die moved either way by the amount, then none.
  "AdjustDieRoll" -> do
    options <- Protocol.field "options" offer >>= Protocol.arrayOf
    results <- Protocol.field "results" offer >>= Protocol.arrayOf
    moved <- Protocol.field "amount" offer >>= Protocol.naturalOf
    let several = length results > 1
        name option = case option of
          R.Array [R.Number index, R.Text way]
            | Just (R.Number result) <- Maybe.listToMaybe (List.genericDrop index results),
              Just sign <- List.lookup way [(Text.pack "Increase", 1), (Text.pack "Decrease", -1)] ->
                Text.concat [if several then Text.pack ("Die " <> show (index + 1) <> ": ") else Text.empty, way, Text.pack (" " <> show result <> " to " <> show (result + sign * toInteger moved))]
          _ -> Offer.optionName view option
    pure
      ( Offer.told view Text.empty offer <> (kindHeading <> Text.pack ":" : numbered (fmap name options)),
        fmap (pickFrom options) . one (Natural.length options)
      )
  "ChooseOptional" -> do
    source <- Protocol.field "source" offer
    pure
      ( [Offer.objectName view source <> Text.pack ": use its optional effect?", Text.pack "  1. Yes", Text.pack "  2. No"],
        fmap (\i -> optionalDecision (if i == 1 then OptionalDecision.Exercises else OptionalDecision.Declines)) . one 2
      )
  -- CR 116.2c / 605.3a: Channel's life for mana, inside a payment.
  "PayLifeForMana" -> do
    life <- Protocol.field "life" offer >>= Protocol.naturalOf
    mana <- Protocol.field "mana" offer >>= Offer.decoded Codec.ManaType.codec
    pure
      ( [Text.pack ("Pay " <> show life <> " life to add ") <> Render.manaType mana <> Text.pack "?", Text.pack "  1. Yes", Text.pack "  2. No"],
        fmap (\i -> optionalDecision (if i == 1 then OptionalDecision.Exercises else OptionalDecision.Declines)) . one 2
      )
  -- CR 510.1b-c: whether the creature assigns its damage as its option allows.
  "ChooseCombatDamageOption" -> do
    source <- Protocol.field "source" offer
    option <- Protocol.field "option" offer >>= Offer.decoded Codec.CombatDamageOption.codec
    let wording = if option == CombatDamageOption.AsThoughUnblocked then Text.pack ": assign its combat damage as though it weren't blocked?" else Text.pack ": assign its combat damage to a creature the defending player controls?"
    pure
      ( [Offer.objectName view source <> wording, Text.pack "  1. Yes", Text.pack "  2. No"],
        fmap (\i -> optionalDecision (if i == 1 then OptionalDecision.Exercises else OptionalDecision.Declines)) . one 2
      )
  -- CR 510.1c-d: one amount per recipient, in the order offered, beside how
  -- much of its lethal damage other creatures already assign (CR 702.19b-c).
  "AssignCombatDamage" -> do
    source <- Protocol.field "source" offer
    lethal <- Protocol.field "lethal" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf
    paid <- Protocol.field "paid" offer >>= Protocol.arrayOf >>= mapM Protocol.entryOf
    total <- Protocol.field "total" offer >>= Protocol.naturalOf
    let assigned recipient = case List.lookup recipient paid of
          Just n | n > 0 -> ", " <> show n <> " already assigned"
          _ -> ""
        needs recipient threshold = if threshold > 0 then Text.pack (" (lethal " <> show threshold <> assigned recipient <> ")") else Text.empty
        wrong = Text.pack ("Type " <> show (length lethal) <> " amounts that add up to " <> show total <> ".")
    pure
      ( Text.concat [Text.pack "Divide ", Offer.objectName view source, Text.pack ("'s " <> show total <> " damage, one amount each, separated by spaces:")]
          : numbered (fmap (\(recipient, threshold) -> Offer.recipientName view recipient <> needs recipient threshold) lethal),
        \typed -> case numbers typed of
          Right amounts
            | length amounts == length lethal && sum amounts == total ->
                Right (R.Array [R.Array [recipient, R.Number (toInteger amount)] | ((recipient, _), amount) <- zip lethal amounts, amount > 0])
          _ -> Left wrong
      )
  -- CR 601.2d / 115.7f: one amount per target, in the order offered.
  "ChooseDivision" -> do
    source <- Protocol.field "source" offer
    targets <- Protocol.field "targets" offer >>= Protocol.arrayOf
    let fixed = Protocol.field "parts" offer >>= Protocol.arrayOf >>= mapM Protocol.naturalOf
    total <- maybe (Protocol.field "total" offer >>= Protocol.naturalOf) (Just . sum) fixed
    let wrong = Text.pack ("Type " <> show (length targets) <> " amounts of at least 1 that add up to " <> show total <> maybe "" (\parts -> ", using exactly " <> unwords (fmap show parts)) fixed <> ".")
        fits amounts = length amounts == length targets && all (>= 1) amounts && sum amounts == total && all ((== List.sort amounts) . List.sort) fixed
    pure
      ( Text.concat [Text.pack "Divide ", Offer.objectName view source, Text.pack ("'s " <> show total <> ", one amount per target, separated by spaces:")]
          : numbered (fmap (Offer.recipientName view) targets),
        \typed -> case numbers typed of
          Right amounts | fits amounts -> Right (R.Array [R.Array [recipient, R.Number (toInteger amount)] | (recipient, amount) <- zip targets amounts])
          _ -> Left wrong
      )
  -- CR 119.7: for each player in turn, the player whose life total they take.
  "ChooseRedistribution" -> do
    totals <- Protocol.field "totals" offer >>= Protocol.arrayOf >>= mapM (\total -> case total of R.Array [player, R.Number life] -> Just (player, life); _ -> Nothing)
    let players = fmap fst totals
        size = Natural.length players
    pure
      ( Text.pack "Redistribute life totals: for each player in turn, type the number of the player whose life total they take, separated by spaces, or press Enter to keep every total:"
          : numbered [Offer.playerName player <> Text.pack (" (" <> show life <> " life)") | (player, life) <- totals],
        \typed -> case numbers typed of
          Right [] -> Right (R.Array [])
          Right picked | List.sort picked == [1 .. size] -> Right (R.Array [R.Array [player, giver] | (player, i) <- zip players picked, let giver = pickFrom players i, giver /= player])
          _ -> Left (Text.pack ("Type each number from 1 to " <> show size <> " once, or press Enter."))
      )
  -- CR 603.3b / 101.4c: first to last, the first named going on the stack
  -- first; Enter keeps the order shown.
  "OrderTriggers" -> do
    entries <- Protocol.arrayOf offer
    pure (ordered (Text.pack "Order your triggered abilities: type their numbers first to last, separated by spaces, or press Enter to keep this order. On the stack, the last resolves first:") (fmap (Offer.triggerName view) entries))
  -- CR 601.2h: each component as Oracle text prints it, the first paid first.
  "OrderCostComponents" -> do
    source <- Protocol.field "source" offer
    components <- Protocol.field "order" offer >>= Protocol.arrayOf
    let self = Offer.objectName view source
    pure (ordered (orderHeading (Text.pack " for " <> self) (Text.pack "the first is paid first")) (fmap (componentName self) components))
  -- CR 601.2h: the pool's unit that pays the next symbol, sent back whole.
  "ChooseManaToSpend" -> do
    units <- Protocol.arrayOf offer
    shown <- mapM (Offer.decoded Codec.ManaUnit.codec) units
    pure
      ( Text.pack "Choose the mana to spend:" : numbered (fmap Offer.unitName shown),
        fmap (pickFrom units) . one (Natural.length units)
      )
  -- CR 701.23b: up to the count, which may be none.
  "Search" -> do
    from <- Protocol.field "from" offer >>= Protocol.arrayOf
    names <- Protocol.field "names" offer >>= Protocol.arrayOf >>= mapM Offer.textOf
    wanted <- Protocol.field "count" offer >>= Protocol.naturalOf
    let n = min wanted (Natural.length from)
    pure
      ( Text.pack "Choose up to " <> Render.count n "card" <> Text.pack " to find, by number or range such as 1-3, separated by spaces, or press Enter for none:" : numbered names,
        \typed -> do
          picked <- some False from typed
          if Natural.length picked <= n then Right (R.Array picked) else Left (Text.pack ("Choose at most " <> show n <> "."))
      )
  -- CR 700.2: each mode as the offer names it, or else by its place in the
  -- printed list.
  "ChooseModes" -> subset (\reply -> maybe (Offer.optionName view reply) (\i -> Text.pack ("Mode " <> show (i + 1))) (Protocol.naturalOf reply)) Text.empty R.Array
  -- CR 702.47b: each card beside the splice cost it adds, the effects
  -- happening in the order the cards are typed.
  "ChooseSplice" -> do
    among <- Protocol.field "among" offer >>= Protocol.arrayOf
    let costs = Maybe.fromMaybe [] (Protocol.field "costs" offer >>= Protocol.arrayOf)
        spliced card = Offer.optionName view card <> foldMap (\cost -> Text.pack " (splice for " <> Offer.candidateName (Offer.objectName view card) cost <> Text.pack ")") (lookup card (zip among costs))
    subset spliced (Text.pack "; the first typed happens first") R.Array
  -- CR 701.34a: the permanents picked, then the players.
  "ChooseProliferate" ->
    subset (Offer.optionName view) Text.empty (\picked -> R.Array [R.Array (filter (Maybe.isJust . Offer.objectOf) picked), R.Array [player | R.Array [R.Text tag, player] <- picked, tag == Text.pack "Player"]])
  -- CR 702.22c: each band its attackers' numbers joined by +, each attacker in
  -- at most one.
  "DeclareBands" -> do
    among <- Protocol.field "among" offer >>= Protocol.arrayOf
    let size = Natural.length among
        band word = traverse (either (const Nothing) Maybe.listToMaybe . numbers) (Text.splitOn (Text.pack "+") word)
        fits groups = let is = concat groups in all ((>= 2) . length) groups && all (\i -> i >= 1 && i <= size) is && ListUtils.nubOrd is == is
    pure
      ( Text.pack "Declare bands: type each band as its attackers' numbers joined by +, such as 1+2, separated by spaces, or press Enter for none:" : numbered (fmap (Offer.listedName view) among),
        \typed -> case traverse band (Text.words typed) of
          Just groups | fits groups -> Right (R.Array (fmap (R.Array . fmap (pickFrom among)) groups))
          _ -> Left (Text.pack ("Type each band as two or more numbers from 1 to " <> show size <> " joined by +, such as 1+2, each number at most once."))
      )
  -- CR 701.56a: each object chosen by its number, then + to put a time counter
  -- on it or - to remove one.
  "ChooseTimeTravel" -> do
    among <- Protocol.field "among" offer >>= Protocol.arrayOf
    let size = Natural.length among
        entry word = do
          (digits, sign) <- Text.unsnoc word
          i <- either (const Nothing) Maybe.listToMaybe (numbers digits)
          way <- lookup sign [('+', "Add"), ('-', "Remove")]
          Monad.guard (Text.all Char.isDigit digits && i >= 1 && i <= size)
          pure (i, way)
    pure
      ( Text.concat [Text.pack "Time travel for ", foldMap (Offer.objectName view) (Protocol.field "source" offer), Text.pack ": type each number followed by + to put a time counter on it or - to remove one, such as 1+ 2-, separated by spaces, or press Enter for none:"] : numbered (fmap (Offer.listedName view) among),
        \typed -> case traverse entry (Text.words typed) of
          Just chosen | ListUtils.nubOrd (fmap fst chosen) == fmap fst chosen -> Right (R.Array [R.Array [pickFrom among i, R.Text (Text.pack way)] | (i, way) <- chosen])
          _ -> Left (Text.pack ("Type numbers from 1 to " <> show size <> ", each once, followed by + or -, such as 1+ 2-."))
      )
  -- CR 701.22a: the bottom pile reads from the bottom inward.
  "ChooseScry" -> piles (Text.pack "Scry") (Text.pack "on the bottom of your library, the bottommost first")
  -- CR 701.25a / 404.3: the graveyard pile in the order it is put there.
  "ChooseSurveil" -> piles (Text.pack "Surveil") (Text.pack "into your graveyard, the first put there first")
  -- CR 701.29a: as a scry of another's library.
  "ChooseFateseal" -> piles (Text.pack "Fateseal " <> foldMap (\owner -> Offer.playerName owner <> Text.pack "'s library") (Protocol.field "owner" offer)) (Text.pack "on the bottom of that library, the bottommost first")
  -- An order over what the offer lists, sent back as a permutation of its
  -- indices.
  _
    | Just items <- Protocol.field "order" offer >>= Protocol.arrayOf ->
        pure (ordered (orderHeading (foldMap (\source -> Text.pack " for " <> Offer.objectName view source) (Protocol.field "source" offer)) (Offer.orderNote (Question.kind question) offer)) (fmap (Offer.optionName view) items))
  -- An amount per item the offer lists, each within its range.
  _ | Just caps <- Protocol.field "amounts" offer -> pure (amounted caps)
  -- A set of what the offer lists, within its least and most, each beside
  -- what it counts toward a threshold where the offer says.
  _ | Just among <- Protocol.field "among" offer >>= Protocol.arrayOf -> do
    let tallies = Maybe.fromMaybe [] (Protocol.field "counts" offer >>= Protocol.arrayOf)
        tallied candidate = Offer.optionName view candidate <> foldMap (\n -> Text.pack " (counts as " <> Offer.optionName view n <> Text.pack ")") (lookup candidate (zip among tallies))
    subset tallied Text.empty R.Array
  -- One of what the offer lists, sent back as the answer it already is, and
  -- named by its entry where the offer lists one.
  _ -> do
    (options, name) <- Monad.mplus (fmap (\o -> (o, Offer.optionName view)) (Protocol.field "options" offer >>= Protocol.arrayOf)) (fmap (\o -> (o, Offer.playerName)) (Protocol.field "players" offer >>= Protocol.arrayOf))
    let self = foldMap (Offer.objectName view) (Protocol.field "source" offer)
        entries = foldMap (fmap Just) (Protocol.field "entries" offer >>= Protocol.arrayOf) <> repeat Nothing
    pure
      ( Offer.told view self offer
          <> (kindHeading <> foldMap (\source -> Text.pack " for " <> Offer.objectName view source) (Protocol.field "source" offer) <> Text.pack ":" : numbered (zipWith (\option entry -> maybe (Maybe.fromMaybe (name option) (gone option)) (Offer.entryName view) entry) options entries)),
        fmap (pickFrom options) . one (Natural.length options)
      )
  where
    offer = Question.offer question
    -- CR 608.2h: an option that has left the game, by the last known name the
    -- offer gives it (Parun.Scenario.PromptOffer.departed).
    gone option = do
      key <- Offer.textOf option
      known <- Protocol.field "departed" offer >>= Protocol.field (Text.unpack key) >>= Offer.textOf
      pure (known <> Text.pack " (it is gone)")
    -- A number from the offer's least to its greatest, for its source.
    bounded heading = do
      source <- Protocol.field "source" offer
      least <- Protocol.field "least" offer >>= Protocol.naturalOf
      greatest <- Protocol.field "greatest" offer >>= Protocol.naturalOf
      pure (ranged (heading <> Offer.objectName view source) least Text.empty (Just greatest))
    -- A number from least to greatest, or from least up with no greatest.
    ranged heading least note greatest =
      let range = maybe ("at least " <> show least) (\most -> "from " <> show least <> " to " <> show most) greatest
       in ( [heading <> Text.pack (", " <> range) <> note <> Text.pack ":"],
            \typed -> case Read.readMaybe (Text.unpack (Text.strip typed)) of
              Just n | least <= n && all (n <=) greatest -> Right (R.Number (toInteger n))
              _ -> Left (Text.pack ("Type a number " <> maybe "of " (const "") greatest <> range <> "."))
          )
    -- A source by number, or the line `none` for no source.
    sourcePick heading none others = do
      offered <- Protocol.field "sources" offer >>= Protocol.arrayOf >>= mapM Offer.manaSource
      let sources = fmap fst offered
          makes yields = if null yields then Text.empty else Text.pack " (makes " <> Text.intercalate (Text.pack " or ") (fmap (foldMap Render.manaType) yields) <> Text.pack ")"
      pure
        ( heading : numbered (fmap (\(source, yields) -> Offer.listedName view source <> makes yields) offered),
          \typed ->
            if Text.toCaseFold typed == none
              then Right R.Null
              else either (const (Left (Text.pack ("Type one number from 1 to " <> show (Natural.length sources)) <> others <> Text.pack "."))) (Right . pickFrom sources) (one (Natural.length sources) typed)
        )
    -- A set of what the offer lists under "among", within its least and most,
    -- each named as the offer names it or else by `name`, a pick repeating
    -- where the offer allows it (CR 700.2d), and sent back as `finish` makes it.
    subset name note finish = do
      among <- Protocol.field "among" offer >>= Protocol.arrayOf
      let least = Maybe.fromMaybe 0 (Protocol.field "least" offer >>= Protocol.naturalOf)
          most = Protocol.field "most" offer >>= Protocol.naturalOf
          repeats = Protocol.field "repeats" offer == Just (R.Boolean True)
          bound = case most of
            Just m | m == least -> "exactly " <> show m
            Just m -> "from " <> show least <> " to " <> show m
            Nothing | least > 0 -> "at least " <> show least
            Nothing -> "any number"
          self = foldMap (Offer.objectName view) (Protocol.field "source" offer)
          again = if repeats then Text.pack "; one may be chosen more than once" else Text.empty
          none = if least == 0 then ", or press Enter for none" else ""
      pure
        ( Offer.told view self offer
            <> (Text.concat [kindHeading, foldMap (const (Text.pack " for " <> self)) (Protocol.field "source" offer), Text.pack (": choose " <> bound <> ", by number or range such as 1-3, separated by spaces"), note, again, Text.pack (none <> ":")] : numbered (zipWith (Maybe.fromMaybe . name) among offeredNames)),
          \typed -> do
            picked <- some repeats among typed
            let n = Natural.length picked
            if n >= least && all (n <=) most then Right (finish picked) else Left (Text.pack ("Choose " <> bound <> "."))
        )
    -- One amount per item under "amounts" (Parun.Scenario.PromptOffer.amounts), in
    -- the order listed, within the offer's bounds (Parun.Scenario.PromptOffer.amountsFit).
    -- Enter is none of each. Sent back in the offer's shape, a
    -- map leaving out each key that takes none.
    amounted caps =
      let leaves = Protocol.amountLeaves caps
          floors = Map.fromList (foldMap Protocol.amountLeaves (Protocol.field "floors" offer))
          floorOf path = Map.findWithDefault 0 path floors
          least = Protocol.field "least" offer >>= Protocol.naturalOf
          most = Protocol.field "most" offer >>= Protocol.naturalOf
          supply = foldMap (Maybe.mapMaybe Protocol.entryOf) (Protocol.field "counters" offer >>= Protocol.arrayOf)
          fromOne = Protocol.field "fromOne" offer == Just (R.Boolean True)
          oneKind = Protocol.field "oneKind" offer >>= Protocol.naturalOf
          entries = foldMap (fmap Just) (Protocol.field "entries" offer >>= Protocol.arrayOf) <> repeat Nothing
          self = foldMap (Offer.objectName view) (Protocol.field "source" offer)
          step key = case key of
            Left i -> maybe (Text.pack ("Item " <> show (i + 1))) (Offer.optionName view) (Monad.join (Maybe.listToMaybe (List.genericDrop i entries)))
            Right k -> Offer.amountKeyName view k
          range (path, cap) = if floorOf path > 0 then "from " <> show (floorOf path) <> " to " <> show cap else "up to " <> show cap
          shown (path, cap) = Text.intercalate (Text.pack ": ") (fmap step path) <> Text.pack (" (" <> range (path, cap) <> ")")
          total = case (least, most) of
            (Just l, Just m) | l == m -> ", adding up to exactly " <> show m
            (Just l, Just m) -> ", adding up to from " <> show l <> " to " <> show m
            (Just l, Nothing) -> ", adding up to at least " <> show l
            (Nothing, Just m) -> ", adding up to at most " <> show m
            (Nothing, Nothing) -> ""
          rules =
            [", all from one" | fromOne]
              <> foldMap (\n -> [", all of one kind: " <> show n <> " of it, or all of it if fewer"]) oneKind
              <> [", each kind in all at most what there is: " <> Text.unpack (Text.intercalate (Text.pack ", ") [Text.pack (show held <> " ") <> Offer.amountKeyName view kind | (kind, held) <- supply]) | not (null supply)]
          fits = Protocol.amountsFit offer
          none = replicate (length leaves) (0 :: Natural)
          enter = if fits none then ", or press Enter for none" else ""
       in ( Offer.told view self offer
              <> (Text.concat [kindHeading, foldMap (const (Text.pack " for " <> self)) (Protocol.field "source" offer), Text.pack (": type one amount for each, separated by spaces" <> total <> concat rules <> enter <> ":")] : numbered (fmap shown leaves)),
            \typed -> case numbers typed of
              Right [] | fits none -> Right (Offer.fillAmounts caps none)
              Right picked | fits picked -> Right (Offer.fillAmounts caps picked)
              _ -> Left (Text.pack ("Type " <> show (length leaves) <> " amounts, each within its range" <> total <> concat rules <> "."))
          )
    -- Two ordered piles of the cards under "from": those for the top, top
    -- first, then after a slash those for the other pile, each card once;
    -- Enter keeps them all on top as listed. Sent back as (other, top).
    piles heading other = do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      let size = Natural.length from
          pick = R.Array . fmap (pickFrom from)
      pure
        ( Text.concat [heading, Text.pack ": type the numbers of the cards to keep on top, the top first, then / and the numbers of those to put ", other, Text.pack ", or press Enter to keep them all on top in this order:"]
            : numbered (zipWith (Maybe.fromMaybe . Offer.listedName view) from offeredNames),
          \typed -> case traverse numbers (Text.splitOn (Text.pack "/") typed) of
            _ | Text.null (Text.strip typed) -> Right (R.Array [R.Array [], R.Array from])
            Right [top] | List.sort top == [1 .. size] -> Right (R.Array [R.Array [], pick top])
            Right [top, rest] | List.sort (top <> rest) == [1 .. size] -> Right (R.Array [pick rest, pick top])
            _ -> Left (Text.pack ("Type each number from 1 to " <> show size <> " once, those for the top first and a / before the rest, or press Enter."))
        )
    -- The names the offer lists under "names", then none.
    offeredNames = foldMap (fmap Offer.textOf) (Protocol.field "names" offer >>= Protocol.arrayOf) <> repeat Nothing
    -- The prompt's kind as words, capitalized.
    kindHeading = case Text.uncons (Render.spaced (Question.kind question)) of
      Just (c, rest) -> Text.cons (Char.toUpper c) rest
      Nothing -> Text.empty
    orderHeading for note = Text.concat [kindHeading, for, Text.pack ": type their numbers first to last, separated by spaces, or press Enter to keep this order; ", note, Text.pack ":"]
    pickCount purpose = do
      from <- Protocol.field "from" offer >>= Protocol.arrayOf
      wanted <- Protocol.field "count" offer >>= Protocol.naturalOf
      let n = min wanted (Natural.length from)
      pure
        ( Text.pack "Choose " <> Render.count n "card" <> Text.pack (" " <> purpose <> ", by number or range such as 1-3, separated by spaces:") : numbered (fmap (Offer.listedName view) from),
          fmap R.Array . exactly n from
        )
    generic =
      ( [Render.spaced (Question.kind question) <> Text.pack ":"]
          <> [Common.render (Codec.Reply.toValue offer) | offer /= R.Null]
          <> [Text.pack "Type the answer as JSON:"],
        Common.parse Monad.>=> Codec.Reply.fromValue
      )

-- | What "card" followed by a number from the question's list, or by a name,
-- shows: each object of the view it names, in full. Only what the view shows
-- is read, so a card the seat may not see is never found. Left when the card
-- data could not say, which stops play.
reading :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerView.PlayerView -> [Maybe ObjectId.ObjectId] -> Text.Text -> Either InvalidCorpus.InvalidCorpus [Text.Text]
reading lookUp view listed wanted
  | Text.null wanted = Right [Text.pack "Type card and a number from a list, or a card's name."]
  | Right [i] <- numbers wanted = case Monad.join (Maybe.listToMaybe (List.genericDrop (pred (max 1 i)) listed)) >>= (`Map.lookup` Render.objects view) of
      Just o | i >= 1 -> Render.details lookUp view o
      _ -> Right [Text.pack ("Number " <> show i <> " in this list is not a card you can see.")]
  | otherwise =
      flip fmap (readingByName lookUp view wanted) $ \found ->
        if null found then [Text.pack "No card you can see is named " <> wanted <> Text.pack "."] else found

-- | Each object of the view named this, ignoring case, in full; none when
-- no card the seat can see is.
readingByName :: (CardName.CardName -> Either InvalidCorpus.InvalidCorpus (Maybe Card.Card)) -> PlayerView.PlayerView -> Text.Text -> Either InvalidCorpus.InvalidCorpus [Text.Text]
readingByName lookUp view wanted =
  fmap (List.intercalate [Text.empty] . ListUtils.nubOrd) (traverse (Render.details lookUp view) (filter matches (Map.elems (Render.objects view))))
  where
    matches = any ((== Text.toCaseFold (Text.strip wanted)) . Text.toCaseFold . CardName.unwrap) . Set.toList . Render.cardNames

-- | The object behind each numbered option of an offer, where it names one:
-- the offer's own list, the cards, sources, entries or options it chooses
-- from, what it orders or picks a set among, or what it asks an amount of.
listedObjects :: PlayerView.PlayerView -> R.Reply -> [Maybe ObjectId.ObjectId]
listedObjects view offer = foldMap (fmap optionObject) (Monad.msum [fmap (fmap snd . Offer.distinct view) (Protocol.arrayOf offer), Protocol.field "from" offer >>= Protocol.arrayOf, Protocol.field "sources" offer >>= Protocol.arrayOf, Protocol.field "entries" offer >>= Protocol.arrayOf, Protocol.field "options" offer >>= Protocol.arrayOf, Protocol.field "order" offer >>= Protocol.arrayOf, Protocol.field "among" offer >>= Protocol.arrayOf, fmap (fmap (Either.fromRight R.Null . Maybe.fromMaybe (Left 0) . Maybe.listToMaybe . fst) . Protocol.amountLeaves) (Protocol.field "amounts" offer)])
  where
    optionObject option = Monad.msum [Offer.objectOf option, Protocol.field "object" option >>= Offer.objectOf, Protocol.field "source" option >>= Offer.objectOf]

-- | What to say, and the answer the seat opted into ahead of the question,
-- if any: passing a priority that offers nothing but passing and
-- mana abilities, and the rest of a payment the seat chose auto at. A
-- payment's other prompts and a refused answer end "auto", and a refused
-- answer is always asked. A priority is passed for only the seat the screen
-- faces: another seat is handed the screen and asked, whatever it holds, so
-- neither a hand-off nor a line tells the table it had nothing else to do.
unasked :: (Monad m) => PlayerView.PlayerView -> Request.Request -> Client m ([Text.Text], Maybe R.Reply)
unasked view request = do
  let question = Request.question request
      seat = Question.seat question
      offer = Question.offer question
      kind = Text.unpack (Question.kind question)
      refused = Maybe.isJust (Request.problem request)
  hotseat <- State.get
  let paying = Map.lookup seat (Hotseat.paying hotseat)
  Monad.when (refused || kind `notElem` ["ChooseManaSource", "ChooseExtraManaSource", "PayLifeForMana", "ChooseManaYield", "ChooseManaToSpend"]) (stopPaying seat)
  case kind of
    _ | refused -> pure ([], Nothing)
    "ChooseAction"
      | Hotseat.autoPass hotseat,
        Hotseat.facing hotseat == Just seat,
        Just pass <- Offer.onlyPass offer ->
          let active = PlayerView.active view
              whose = if active == seat then Text.pack "their " else Render.player active <> Text.pack "'s "
           in pure ([Render.player seat <> Text.pack " passes in " <> whose <> Render.phase (PlayerView.phase view) <> Text.pack " (no other actions)."], Just (R.Number pass))
    "ChooseManaSource"
      | Maybe.isJust paying ->
          autoPay seat view offer >>= \paid -> case paid of
            Just (said, reply) -> pure ([said], Just reply)
            Nothing -> stopPaying seat >> pure ([Text.pack "Auto can't pay the rest of this cost."], Nothing)
    -- CR 605.3b: the mana the source was tapped for.
    "ChooseManaYield"
      | Just wanted <- paying,
        Just yields <- Protocol.field "yields" offer >>= Offer.decoded (Common.list (Common.list Codec.ManaType.codec)),
        (i, yield) : _ <- filter (\(_, yield) -> Set.null wanted || not (Set.disjoint wanted (Set.fromList yield))) (zip [0 :: Integer ..] yields) ->
          pure ([Text.pack "Auto takes " <> foldMap Render.manaType yield <> Text.pack "."], Just (R.Number i))
    -- CR 116.2c: auto taps sources; it never spends the seat's life.
    "PayLifeForMana" | Maybe.isJust paying -> pure ([], Just (optionalDecision OptionalDecision.Declines))
    -- CR 605.3a: auto never floats mana past the cost.
    "ChooseExtraManaSource" | Maybe.isJust paying -> stopPaying seat >> pure ([], Just R.Null)
    _ -> pure ([], Nothing)

-- | Auto's next source for the seat, and the line saying so, remembering the
-- colors the source is tapped for.
autoPay :: (Monad m) => PlayerId.PlayerId -> PlayerView.PlayerView -> R.Reply -> Client m (Maybe (Text.Text, R.Reply))
autoPay seat view offer = case Offer.autoSource view offer of
  Nothing -> pure Nothing
  Just (source, wanted) -> do
    State.modify' (\h -> h {Hotseat.paying = Map.insert seat wanted (Hotseat.paying h)})
    pure (Just (Text.pack "Auto pays with " <> Offer.objectName view source <> Text.pack ".", source))

stopPaying :: (Monad m) => PlayerId.PlayerId -> Client m ()
stopPaying seat = State.modify' (\h -> h {Hotseat.paying = Map.delete seat (Hotseat.paying h)})

-- | CR 101.4b: a line for each earlier payer an offer lists under
-- @"earlier"@, saying what they paid, with what, or that they declined.
payersTold :: PlayerView.PlayerView -> Text.Text -> R.Reply -> [Text.Text]
payersTold view self offer = foldMap (fmap line) (Protocol.field "earlier" offer >>= Protocol.arrayOf)
  where
    line entry = case entry of
      R.Array [player, R.Null] -> Offer.playerName player <> Text.pack " declined."
      R.Array [player, R.Array [cost, taken]] ->
        Text.concat [Offer.playerName player, Text.pack " paid ", Offer.candidateName self cost, foldMap (\xs -> if null xs then Text.empty else Text.pack " with " <> Text.intercalate (Text.pack ", ") (fmap (Offer.listedName view) xs)) (Protocol.arrayOf taken), Text.pack "."]
      _ -> Offer.optionName view entry

-- | An order over the items shown, answered by their numbers first to last,
-- each once, or by Enter for the order shown; sent back as a permutation of
-- their indices (Parun.Engine.Game.permute).
ordered :: Text.Text -> [Text.Text] -> ([Text.Text], Text.Text -> Either Text.Text R.Reply)
ordered heading names =
  ( heading : numbered names,
    \typed -> case numbers typed of
      Right [] -> Right (R.Array (fmap (R.Number . toInteger) [0 .. size - 1]))
      Right picked | List.sort picked == [1 .. size] -> Right (R.Array (fmap (R.Number . toInteger . subtract 1) picked))
      _ -> Left (Text.pack ("Type each number from 1 to " <> show size <> " once, or press Enter."))
  )
  where
    size = Natural.length names

-- | A cost component as Oracle text prints it (Oracle.Cost.component), or the
-- offer itself where it does not print.
componentName :: Text.Text -> R.Reply -> Text.Text
componentName self component =
  Maybe.fromMaybe (Common.render (Codec.Reply.toValue component)) (Offer.decoded (Codec.CostComponent.codec Codec.Keyword.codec) component >>= Maybe.listToMaybe . Oracle.Cost.component self (Oracle.Keyword.keywordWord self))

-- | An optional prompt's answer, a bare tag (Parun.JsonCodec.Arm.enum).
optionalDecision :: OptionalDecision.OptionalDecision -> R.Reply
optionalDecision = R.Text . Text.pack . show

-- | One number from 1 to n.
one :: Natural -> Text.Text -> Either Text.Text Natural
one n typed = case numbers typed of
  Right [i] | i >= 1 && i <= n -> Right i
  _ -> Left (Text.pack ("Type one number from 1 to " <> show n <> "."))

-- | Exactly n different options, by number.
exactly :: Natural -> [R.Reply] -> Text.Text -> Either Text.Text [R.Reply]
exactly n options typed = do
  picked <- some False options typed
  if Natural.length picked == n then Right picked else Left (Text.pack ("Choose exactly " <> show n <> "."))

-- | Any number of options, by number, by a range such as 1-3, or all of them;
-- each at most once unless `repeats`.
some :: Bool -> [R.Reply] -> Text.Text -> Either Text.Text [R.Reply]
some repeats options typed = do
  let size = Natural.length options
      range word = case Text.splitOn (Text.pack "-") word of
        _ | Text.toCaseFold word == Text.pack "all" -> Just [1 .. size]
        [from, to] -> do
          a <- digits from
          b <- digits to
          Monad.guard (a <= b)
          pure [a .. b]
        _ -> fmap pure (digits word)
      digits t = if not (Text.null t) && Text.all Char.isDigit t then Read.readMaybe (Text.unpack t) else Nothing
  is <- maybe (Left (Text.pack "Type numbers or ranges such as 1-3, separated by spaces, or all.")) (Right . concat) (mapM range (Text.words typed))
  Monad.unless (all (\i -> i >= 1 && i <= size) is) (Left (Text.pack ("Each number must be from 1 to " <> show size <> ".")))
  Monad.unless (repeats || ListUtils.nubOrd is == is) (Left (Text.pack "Choose each one at most once."))
  pure (fmap (pickFrom options) is)

-- | CR 509.1a: blocker:attacker pairs, a blocker naming each attacker it blocks.
blocks :: [R.Reply] -> [R.Reply] -> Text.Text -> Either Text.Text R.Reply
blocks blockers attackers typed = do
  pairs <- mapM pairOf (Text.words typed)
  let grouped = Map.fromListWith (flip (<>)) (fmap (\(b, a) -> (b, [a])) pairs)
  pure (R.Array (fmap (\(b, as) -> R.Array [pickFrom blockers b, R.Array (fmap (pickFrom attackers) (ListUtils.nubOrd as))]) (Map.toList grouped)))
  where
    pairOf word = case Text.splitOn (Text.pack ":") word of
      [b, a]
        | Right [bi] <- numbers b,
          Right [ai] <- numbers a,
          bi >= 1 && bi <= Natural.length blockers && ai >= 1 && ai <= Natural.length attackers ->
            Right (bi, ai)
      _ -> Left (Text.pack "Type each block as blocker:attacker, such as 1:2.")

numbers :: Text.Text -> Either Text.Text [Natural]
numbers typed = maybe (Left (Text.pack "Type numbers separated by spaces.")) Right (mapM (Read.readMaybe . Text.unpack) (Text.words typed))

pickFrom :: [R.Reply] -> Natural -> R.Reply
pickFrom options i = Maybe.fromMaybe R.Null (Maybe.listToMaybe (List.genericDrop (i - 1) options))

numbered :: [Text.Text] -> [Text.Text]
numbered = zipWith (\i t -> Text.pack ("  " <> show i <> ". ") <> t) [1 :: Natural ..]

-- | No seat may read another's screen, so a change of seat clears it and
-- waits for the next player to say they are there; whether it did. A seat
-- that has lines left to read says when it is done first.
handOff :: (Monad m) => Terminal.Terminal m -> PlayerId.PlayerId -> Client m Bool
handOff terminal seat = do
  facing <- State.gets Hotseat.facing
  let handing = facing /= Just seat
  Monad.when handing $ do
    unread <- State.gets Hotseat.unread
    Monad.when (Maybe.isJust facing && unread) $ do
      say terminal (Text.pack "Press Enter to pass the screen on.")
      Monad.void (listen terminal)
    lift2 (Terminal.clear terminal)
    say terminal (Text.pack "Pass to " <> Render.player seat <> Text.pack ", then press Enter.")
    _ <- listen terminal
    lift2 (Terminal.clear terminal)
    State.modify' (\h -> h {Hotseat.facing = Just seat})
  pure handing

-- | Adds the view's witnessed entries the client has not read yet to its
-- seat's news ('unseen').
collect :: (Monad m) => PlayerView.PlayerView -> Client m ()
collect view = do
  (seen, unread) <- State.gets (flip unseen view . Hotseat.seen)
  State.modify' $ \h ->
    h
      { Hotseat.seen = seen,
        Hotseat.news = Map.insertWith (flip (<>)) (PlayerView.viewer view) (Seq.fromList unread) (Hotseat.news h)
      }

-- | The view's witnessed entries not read yet, as sentences, and the marks
-- moved past them: the rest of the previous turn's, where the seat last
-- looked during it, and this turn's. Every request's view is read, answered
-- automatically or not, since the view holds only those two turns' entries.
-- A seat is sent a request every turn, even one it is controlled for: each
-- priority asks the seat itself whether it concedes (CR 104.3a, 724.6). The
-- mark is kept per seat and game, since a subgame's and a restarted game's
-- turns are numbered from 1 again (CR 728.1, 730.1a).
unseen :: Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural) -> PlayerView.PlayerView -> (Map.Map (PlayerId.PlayerId, Natural) (Natural, Natural), [Text.Text])
unseen seen view =
  let mark = (PlayerView.viewer view, PlayerView.game view)
      turn = PlayerView.turn view
      entries = PlayerView.witnessed view
      missed = case Map.lookup mark seen of
        Just (t, n)
          | t == turn -> List.genericDrop n entries
          | t + 1 == turn -> List.genericDrop n (PlayerView.witnessedLastTurn view) <> entries
        _ -> entries
   in (Map.insert mark (turn, Natural.length entries) seen, Render.events view missed)

-- | The lines showing the seat its news, emptying it: one line saying what
-- happened since it last looked, where the screen was just handed to it,
-- and otherwise a line each, for a seat that never stopped looking.
tell :: (Monad m) => Bool -> PlayerId.PlayerId -> Client m [Text.Text]
tell handed seat = do
  news <- State.gets (foldMap Foldable.toList . Map.lookup seat . Hotseat.news)
  State.modify' (\h -> h {Hotseat.news = Map.delete seat (Hotseat.news h)})
  pure $
    if handed
      then [Text.pack "Since you last looked: " <> Text.unwords news | not (null news)]
      else news

-- | At the game's end, each person seat's news it was not yet shown (the
-- entries after its last question: the killing blow, say), the seat the
-- screen faces first and any other handed the screen only if it has news.
ending :: (Monad m) => Terminal.Terminal m -> [PlayerId.PlayerId] -> GameState.GameState -> Client m ()
ending terminal people gs = do
  facing <- State.gets Hotseat.facing
  Monad.forM_ (List.sortOn ((/= facing) . Just) people) $ \seat -> do
    collect (PlayerView.Engine.viewFor seat gs)
    pending <- State.gets (maybe False (not . null) . Map.lookup seat . Hotseat.news)
    Monad.when pending $ do
      handed <- handOff terminal seat
      mapM_ (say terminal) =<< tell handed seat

say :: (Monad m) => Terminal.Terminal m -> Text.Text -> Client m ()
say terminal line = do
  lift2 (Terminal.say terminal line)
  State.modify' (\h -> h {Hotseat.unread = True})

listen :: (Monad m) => Terminal.Terminal m -> Client m Text.Text
listen terminal = do
  typed <- lift2 (Terminal.listen terminal)
  State.modify' (\h -> h {Hotseat.unread = False})
  maybe (stop (Text.pack "end of input")) pure typed

stop :: (Monad m) => Text.Text -> Client m a
stop = Trans.lift . Except.throwE

lift2 :: (Monad m) => m a -> Client m a
lift2 = Trans.lift . Trans.lift
