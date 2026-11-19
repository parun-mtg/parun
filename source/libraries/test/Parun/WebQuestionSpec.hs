-- Covers Parun.Web.Question and Parun.Web.Board: a line protocol request as a
-- page, and the reply its taps build, from requests the engine makes.
module Parun.WebQuestionSpec where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Numeric.Natural (Natural)
import qualified Parun.Codec.ManaType as Codec.ManaType
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Extra.Int as Int
import qualified Parun.Extra.Natural as Natural
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Play.Render as Render
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Load as Load
import qualified Parun.Scenario.PromptOffer as PromptOffer
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.AnyNumberOf as AnyNumberOf
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card.Type
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterKindFor as CounterKindFor
import qualified Parun.Types.Daytime as Daytime
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Designation as Designation
import qualified Parun.Types.Dividing as Dividing
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardArrangement as GraveyardArrangement
import qualified Parun.Types.HandActionIndex as HandActionIndex
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Looked as Looked
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.MoveSpread as MoveSpread
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.Stop as Stop
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.WordFamily as WordFamily
import qualified Parun.Types.Zone as Zone
import qualified Parun.Web.Board as Board
import qualified Parun.Web.Card as Card
import qualified Parun.Web.Html as Html
import qualified Parun.Web.Log as Log
import qualified Parun.Web.Question as Question.Web
import qualified Parun.Web.Replay as Replay

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "WebQuestion" $ do
  Spec.it s "a priority offering a land shows the board and a button per action, and a tap answers by index" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addObjectIn Zone.Hand mountain S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass, Action.Play land Nothing])
        view = Question.Web.view request Question.Web.blank
        page = Html.toText view
        tap label = Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertBool s (Text.pack "<button class=\"primary\" type=\"button\">Pass</button>" `Text.isInfixOf` page) "pass"
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (tap "Play Mountain")) (Just (Question.Web.MkReplied (R.Number 1)))
    Spec.assertEqWith s "skip" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (tap "End turn")) (Just (Question.Web.MkReplied (R.Number 0)))
    Spec.assertEqWith s "not automatic" (Question.Web.automatic Question.Web.MkStandard request) Nothing
    Spec.assertBool s (Text.pack "aria-keyshortcuts=\"E\">End turn</button>" `Text.isInfixOf` page) "E ends the turn"
    Spec.assertBool s (Text.pack "Before you pass: you can still play Mountain." `Text.isInfixOf` Html.content view) "a card keeps its name's case"
    -- The hand's Mountain is lit, since the seat may play it, and tapping it reads it.
    Spec.assertBool s (Text.pack "<section class=\"hand\"" `Text.isInfixOf` page) "hand"
    Spec.assertBool s (Text.pack "class=\"card c-land playable\"" `Text.isInfixOf` page) "playable"
    Spec.assertEqWith s "a card in hand is tapped to read it" (Html.search isCard view >>= \path -> Html.handler (Text.pack "click") path view) (Just (Question.Web.MkRead land))
    -- Reading it opens its sheet, whose button plays it.
    let reading = Question.Web.view request Question.Web.blank {Question.Web.reading = Just (Card.MkReading land [Text.pack "({T}: Add {R}.)"] True)}
        sheet = Html.search (\html -> Text.pack "<section class=\"sheet" `Text.isPrefixOf` Html.toText html) reading
    Spec.assertBool s (Text.pack "<span class=\"ms\" title=\"{R}\" role=\"img\" aria-label=\"red\"><svg viewBox=\"0 0 32 32\" aria-hidden=\"true\"><circle class=\"bg-r\" cx=\"16\" cy=\"16\" r=\"16\"></circle><use class=\"fg-r\" href=\"#ms-r\"" `Text.isInfixOf` Html.toText reading) "oracle"
    let action = do
          at <- sheet
          inner <- Html.at at reading
          path <- Html.labelled (Text.pack "button") (Text.pack "Play Mountain") inner
          Html.handler (Text.pack "click") (at <> path) reading
    Spec.assertEqWith s "sheet action" action (Just (Question.Web.MkAnswer (R.Number 1)))

  Spec.it s "CR 702.21a a permanent's sheet lists a keyword too long for its tile" $ do
    dragon <- S.printingOf s registry "Archive Dragon"
    let (oid, gs) = S.addPermanent dragon S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
        board = Html.toText (Question.Web.view request Question.Web.blank)
        reading = Question.Web.view request Question.Web.blank {Question.Web.reading = Just (Card.MkReading oid [] True)}
        sheet = maybe Text.empty Html.toText (Html.search (\html -> Text.pack "<section class=\"sheet" `Text.isPrefixOf` Html.toText html) reading >>= (`Html.at` reading))
    Spec.assertBool s (Text.pack "Keyword abilities: Flying, Ward {2}" `Text.isInfixOf` sheet) "the sheet lists ward"
    Spec.assertBool s (not (Text.pack "Ward {2}" `Text.isInfixOf` board)) "the tile leaves it out"

  Spec.it s "CR 601.2d a division starts one to each target and the rest to the first, and reads as the engine's" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (a, g1) = S.addPermanent bears S.bob (Setup.emptyGame S.bothPlayers)
        (b, g2) = S.addPermanent bears S.bob g1
        (source, gs) = S.addPermanent bears S.alice g2
        prompt = Prompt.ChooseDivision (Decider.MkDecider S.alice) S.alice source (Dividing.Total 3) (NonEmpty.fromList [Recipient.ToCreature a, Recipient.ToCreature b])
        request = requestFor gs prompt
    Spec.assertEqWith s "one each, the rest first" (Question.Web.initial request) (Set.fromList [(0, 1), (1, 0)])
    Spec.assertEqWith s "the reply" (answered prompt request (Question.Web.initial request)) (Right (Map.fromList [(Recipient.ToCreature a, 2), (Recipient.ToCreature b, 1)]))
    Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"amount-row\"") (Html.toText (Question.Web.view request Question.Web.blank))) "a row per target"

  Spec.it s "CR 122.1 an amount per permanent within the offer's total, refused outside it" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (a, g1) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        (b, gs) = S.addPermanent bears S.alice g1
        prompt = Prompt.ChooseCounterRemovalAmong (Decider.MkDecider S.alice) S.alice a 2 (Map.fromList [(a, 3), (b, 1)])
        request = requestFor gs prompt
    Spec.assertEqWith s "none at first" (Question.Web.initial request) (Set.fromList [(0, 0), (1, 0)])
    Spec.assertEqWith s "one from each" (answered prompt request (Set.fromList [(0, 1), (1, 1)])) (Right (Map.fromList [(a, 1), (b, 1)]))
    Spec.assertEqWith s "too few" (Question.Web.step request (Set.fromList [(0, 1), (1, 0)]) Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose amounts adding up to exactly 2."))

  Spec.it s "CR 118.12 a cost to pay is a yes or no naming the cost" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (source, gs) = S.addPermanent bears S.bob (Setup.emptyGame S.bothPlayers)
        prompt = Prompt.ChooseToPay (Decider.MkDecider S.alice) S.alice source PayOffer.InsteadOfDraw (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []) Seq.empty
        request = requestFor gs prompt
        view = Question.Web.view request Question.Web.blank
        tap label = Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertBool s (Text.isInfixOf (Text.pack "Grizzly Bears: pay ") (Html.content view)) "names the cost"
    Spec.assertEqWith s "pay" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (tap "Pay")) (Just (Question.Web.MkReplied (R.Text (Text.pack "Pays"))))
    Spec.assertEqWith s "reads" (Protocol.decodeAnswer prompt (R.Text (Text.pack "Pays"))) (Right PaymentDecision.Pays)

  Spec.it s "CR 601.2b a keyword paid any number of times offers the first few and a field" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (source, gs) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseKicker (Decider.MkDecider S.alice) S.alice source (Keyword.Replicate (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])) Nothing Nothing)
        view = Question.Web.view request Question.Web.blank
        tap label = Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertEqWith s "nine" (tap "9") (Just (Question.Web.MkAnswer (R.Number 9)))
    Spec.assertBool s (Text.isInfixOf (Text.pack "placeholder=\"Another number\"") (Html.toText view)) "a field"
    Spec.assertBool s (not (Question.Web.answersTyped request)) "a form"

  Spec.it s "CR 612.2 a text change picks the word to change and the word it becomes" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (source, gs) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        prompt = Prompt.ChooseWordSwap (Decider.MkDecider S.alice) S.alice source (SlotName.MkSlotName (Text.pack "target")) (Set.singleton WordFamily.Color) Set.empty
        request = requestFor gs prompt
        picks = picking request [Question.Web.MkToggle 0 0, Question.Web.MkToggle 1 1]
    Spec.assertEqWith s "white to blue" (answered prompt request picks) (Right (TextWord.ColorWord Color.White, TextWord.ColorWord Color.Blue))
    Spec.assertEqWith s "two families" (Question.Web.step (requestFor gs (Prompt.ChooseWordSwap (Decider.MkDecider S.alice) S.alice source (SlotName.MkSlotName (Text.pack "target")) (Set.fromList [WordFamily.Color, WordFamily.BasicLandType]) Set.empty)) (Set.fromList [(0, 1), (1, 6)]) Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose two words of the same kind, the new one allowed."))
    Spec.assertEqWith s "the same word twice" (Question.Web.step request (picking request [Question.Web.MkToggle 0 0, Question.Web.MkToggle 1 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose two different words."))

  Spec.it s "CR 700.3 creatures divided into piles start in the first" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (a, g1) = S.addPermanent bears S.bob (Setup.emptyGame S.bothPlayers)
        (b, gs) = S.addPermanent bears S.bob g1
        prompt = Prompt.DividePiles (Decider.MkDecider S.bob) S.bob [a, b] 2
        request = requestFor gs prompt
    Spec.assertEqWith s "all in the first" (answered prompt request (Question.Web.initial request)) (Right [Set.fromList [a, b], Set.empty])
    Spec.assertEqWith s "one moved" (answered prompt request (Set.fromList [(0, 0), (1, 1)])) (Right [Set.singleton a, Set.singleton b])

  Spec.it s "a redistribution of life totals starts with each player keeping their own total" $ do
    let prompt = Prompt.ChooseRedistribution (Decider.MkDecider S.alice) S.alice [(S.alice, 20), (S.bob, 5)]
        request = requestFor (Setup.emptyGame S.bothPlayers) prompt
    Spec.assertEqWith s "kept" (answered prompt request (Question.Web.initial request)) (Right Map.empty)
    Spec.assertEqWith s "swapped" (answered prompt request (Set.fromList [(0, 1), (1, 0)])) (Right (Map.fromList [(S.alice, S.bob), (S.bob, S.alice)]))
    Spec.assertEqWith s "both take one" (Question.Web.step request (Set.fromList [(0, 1), (1, 1)]) Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Give each life total to exactly one player."))

  Spec.it s "CR 404.3 cards going to a graveyard together are ordered from the new top" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (a, g1) = S.addObjectIn Zone.Hand bears S.alice (Setup.emptyGame S.bothPlayers)
        (b, gs) = S.addObjectIn Zone.Hand bears S.alice g1
        prompt = Prompt.ArrangeGraveyardArrivals (Decider.MkDecider S.alice) S.alice [a, b]
        request = requestFor gs prompt
    Spec.assertEqWith s "the second on top" (answered prompt request (picking request [Question.Web.MkToggle 0 1])) (Right (GraveyardArrangement.InOrder [1, 0]))

  Spec.it s "CR 724.1 a choice made for a controlled player says whose it is" $ do
    let gs0 = Setup.emptyGame S.bothPlayers
        gs = gs0 {GameState.turn = (GameState.turn gs0) {TurnRegisters.activePlayer = S.bob}, GameState.control = S.turnControl S.alice S.bob}
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.bob [Action.Pass])
        page = Html.content (Question.Web.view request Question.Web.blank)
    Spec.assertEqWith s "for" (Question.for (Request.question request)) (Just S.bob)
    Spec.assertBool s (Text.isInfixOf (Text.pack "You're deciding for Player 2, whom you control.") page) "banner"
    Spec.assertBool s (Text.isInfixOf (Text.pack "Player 2's turn, which you control.") page) "the turn is the seat's to play"
    Spec.assertBool s (Text.isInfixOf (Text.pack "Pass the screen to Player 1, deciding for Player 2.") (Html.content (Question.Web.handOff request))) "the hand-off says so"
    -- Odric, Master Tactician's chooser decides blocks for a player it does
    -- not control.
    Spec.assertEqWith s "chosen for, not controlled" (Question.for (Request.question (requestFor gs0 (Prompt.DeclareBlockers (Decider.MkDecider S.alice) S.bob [] [])))) Nothing
    Spec.assertEqWith s "the seat's own" (Question.for (Request.question (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])))) Nothing

  Spec.it s "CR 726.1 / 732.1 / 306.5c the table shows the game's designations, player counters, and a walker's loyalty counters" $ do
    liliana <- S.printingOf s registry "Liliana of the Veil"
    let (walker, gs0) = S.addPermanent liliana S.alice (Setup.emptyGame S.bothPlayers)
        gs =
          gs0
            { GameState.monarch = Just S.bob,
              GameState.daytime = Just Daytime.Night,
              GameState.objects = Map.adjust (\o -> o {Object.counters = Map.singleton CounterKind.Loyalty 5}) walker (GameState.objects gs0),
              GameState.players = Map.adjust (\p -> p {Player.counters = Map.singleton PlayerCounterKind.Energy 3}) S.alice (GameState.players gs0)
            }
        page = Html.toText (Question.Web.view (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])) Question.Web.blank)
    Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"count monarch\"") page) "the monarch"
    Spec.assertBool s (Text.isInfixOf (Text.pack "class=\"daytime night\"") page) "night"
    Spec.assertBool s (Text.isInfixOf (Text.pack "<span class=\"k\">Energy</span>3") page) "energy"
    Spec.assertBool s (Text.isInfixOf (Text.pack "title=\"Loyalty\">5</span>") page) "loyalty is its counters"
    Spec.assertBool s (not (Text.isInfixOf (Text.pack "5 loyalty") page)) "not a badge too"

  Spec.it s "the kinds no scenario answers yet have forms too, a companion by its name" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    lutri <- S.printingOf s registry "Lutri, the Spellchaser"
    let (a, g1) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        (b, g2) = S.addPermanent bears S.alice g1
        (card, g3) = S.addObjectIn Zone.Hand bears S.alice g2
        (companion, gs) = Game.intern lutri g3
        decider = Decider.MkDecider S.alice
        plusOne = CounterKind.PlusOnePlusOne
        typedFor :: Prompt.Prompt r -> Bool
        typedFor prompt = Question.Web.answersTyped (requestFor gs prompt)
        companionPage = Html.content (Question.Web.view (requestFor gs (Prompt.ChooseCompanion decider S.alice (pure (OutsideCard.InPool companion)))) Question.Web.blank)
    Spec.assertEqWith
      s
      "typed"
      [ typedFor (Prompt.ChooseCounterKind decider S.alice (CounterKindFor.MovedBetween a b) (pure plusOne)),
        typedFor (Prompt.ChooseMovedCounterOrNone decider S.alice a b (pure plusOne)),
        typedFor (Prompt.ChooseDistributedMovedCounters decider S.alice a MoveSpread.AnyNumber (Map.singleton plusOne 2) (pure b)),
        typedFor (Prompt.ChooseForage decider S.alice a),
        typedFor (Prompt.ChooseAnyNumber decider S.alice a AnyNumberOf.ToSacrifice [a, b] Nothing Seq.empty),
        typedFor (Prompt.KeepHand decider S.alice (pure [card])),
        typedFor (Prompt.MulliganAction decider S.alice [(card, HandActionIndex.MkHandActionIndex 0)]),
        typedFor (Prompt.OpeningHandAction decider S.alice [(card, HandActionIndex.MkHandActionIndex 0)]),
        typedFor (Prompt.ChooseDesignation decider S.alice a (pure Designation.Renowned)),
        typedFor (Prompt.ChooseNamePosition decider S.alice a (pure 1)),
        typedFor (Prompt.ChooseCompanion decider S.alice (pure (OutsideCard.InPool companion)))
      ]
      (replicate 11 False)
    Spec.assertBool s (Text.isInfixOf (Text.pack "Lutri, the Spellchaser") companionPage) ("the companion by name: " <> Text.unpack companionPage)

  Spec.it s "a double-faced card's sheet reads its other face too" $ do
    jace <- S.printingOf s registry "Jace, Vryn's Prodigy"
    let (oid, gs) = S.addPermanent jace S.alice (Setup.emptyGame S.bothPlayers)
        lookUp name = Right (if any ((== name) . Face.name) (Card.Type.faces (Printing.card jace)) then Just (Printing.card jace) else Nothing) :: Either () (Maybe Card.Type.Card)
        shown = List.find ((== oid) . ObjectView.object) (PlayerView.Type.battlefield (PlayerView.viewFor S.alice gs))
        text = maybe (Right []) (Card.oracle lookUp) shown
    Spec.assertBool s (either (const False) (elem (Text.pack "Jace, Telepath Unbound (other face):")) text) ("the back face: " <> show text)
    Spec.assertEqWith s "the face up first" (fmap (take 1) text) (Right [Text.pack "Jace, Vryn's Prodigy:"])

  Spec.it s "CR 604.1 an ability a permanent's own static ability grants it reads as printed: a Sliver and Karlov Watchdog alone"
    . Monad.forM_ ["Karlov Watchdog", "Sentinel Sliver"]
    $ \name -> do
      printing <- S.printingOf s registry name
      let (oid, gs) = S.addPermanent printing S.alice (Setup.emptyGame S.bothPlayers)
          card = Printing.card printing
          lookUp n = Right (if any ((== n) . Face.name) (Card.Type.faces card) then Just card else Nothing) :: Either () (Maybe Card.Type.Card)
          shown = List.find ((== oid) . ObjectView.object) (PlayerView.Type.battlefield (PlayerView.viewFor S.alice gs))
          printedText = foldMap Text.lines (Face.oracleText (NonEmpty.head (Card.Type.faces card)))
      Spec.assertEqWith s (name <> ": the sheet") (fmap (Card.oracle lookUp) shown) (Just (Right printedText))
      -- On alice's turn the grant applies, and the view carries it.
      Spec.assertBool s (Maybe.isJust (shown >>= ObjectView.ownEffects)) (name <> ": its own effects apply")

  Spec.it s "CR 305.7 / 305.6 a land Blood Moon strips reads as the Mountain's intrinsic mana ability" $ do
    tomb <- S.printingOf s registry "Ancient Tomb"
    moon <- S.printingOf s registry "Blood Moon"
    let (oid, gs) = S.addPermanent tomb S.alice (snd (S.addPermanent moon S.bob (Setup.emptyGame S.bothPlayers)))
        lookUp name = Right (if name == Face.name (NonEmpty.head (Card.Type.faces (Printing.card tomb))) then Just (Printing.card tomb) else Nothing) :: Either () (Maybe Card.Type.Card)
        shown = List.find ((== oid) . ObjectView.object) (PlayerView.Type.battlefield (PlayerView.viewFor S.alice gs))
    Spec.assertEqWith s "the sheet" (fmap (Card.oracle lookUp) shown) (Just (Right [Text.pack "({T}: Add {R}.)"]))

  Spec.it s "CR 305.7 a creature land whose static ability Blood Moon may have stripped is marked as changed" $ do
    -- Ashaya makes the Lord a Forest land, and Blood Moon then sets it to
    -- Mountain; the projection still lists the Lord's static ability.
    lord <- S.printingOf s registry "Lord of Atlantis"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    moon <- S.printingOf s registry "Blood Moon"
    let (oid, gs) = S.addPermanent lord S.alice (snd (S.addPermanent ashaya S.alice (snd (S.addPermanent moon S.bob (Setup.emptyGame S.bothPlayers)))))
        lookUp name = Right (if name == Face.name (NonEmpty.head (Card.Type.faces (Printing.card lord))) then Just (Printing.card lord) else Nothing) :: Either () (Maybe Card.Type.Card)
        shown = List.find ((== oid) . ObjectView.object) (PlayerView.Type.battlefield (PlayerView.viewFor S.alice gs))
    Spec.assertEqWith s "the sheet" (fmap (fmap (take 1) . Card.oracle lookUp) shown) (Just (Right [Text.pack "Its abilities have changed; this text may be out of date."]))

  Spec.it s "CR 104.3a the turn bar carries the auto-pass level and the settings button, whose concession passes while the seat may" $ do
    let request = requestFor (Setup.emptyGame S.bothPlayers) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
        asked = Question.Web.view request Question.Web.blank
        watched = Question.Web.watching request Question.Web.blank
        within prefix = Html.search (\node -> Text.pack prefix `Text.isPrefixOf` Html.toText node)
        conceding html = within "<button class=\"settings-button\"" html >>= \path -> Html.handler (Text.pack "concede") path html
        bar html = within "<div class=\"turn-bar" html >>= \path -> Html.at path html
        prompt = within "<section class=\"prompt\"" asked >>= \path -> Html.at path asked
        level html = bar html >>= \inner -> Html.labelled (Text.pack "button") (Text.pack "Auto") inner >>= \path -> Html.handler (Text.pack "click") path inner
    Spec.assertEqWith s "the menu's Concede passes" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (conceding asked)) (Just (Question.Web.MkReplied (R.Number 0)))
    Spec.assertEqWith s "no concession while watching" (conceding watched) Nothing
    Spec.assertEqWith s "the level is chosen in the turn bar" (level asked) (Just (Question.Web.MkLevel Question.Web.MkAutoPass))
    Spec.assertBool s (maybe False (Text.isInfixOf (Text.pack "aria-pressed=\"false\" disabled=\"\">Auto</button>") . Html.toText) (bar watched)) "inert while watching"
    Spec.assertBool s (maybe False (\p -> not (any (`Text.isInfixOf` Html.content p) [Text.pack "Auto-pass", Text.pack "Concede"])) prompt) "the prompt carries neither"

  Spec.it s "CR 602.1a an activated ability is offered by its Oracle text" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (oid, gs) = S.addPermanent sorcerer S.alice (Setup.emptyGame S.bothPlayers)
        request = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice (Action.Pass : fmap (Action.Activate oid) (Projection.abilitiesOf oid gs)))
    -- The tap symbol is drawn, so the text around it is matched.
    Spec.assertBool s (Text.pack ": Prodigal Sorcerer deals 1 damage to any target." `Text.isInfixOf` Html.content (Question.Web.view request Question.Web.blank)) "labelled"
    -- The end-of-turn reminder names the activation by its source, as a phrase.
    Spec.assertBool s (Text.pack "Before you pass: you can still activate Prodigal Sorcerer." `Text.isInfixOf` Html.content (Question.Web.view request Question.Web.blank)) "reminded"

  Spec.it s "options the seat can't tell apart share one button, which picks the next" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    let (one, g1) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        (two, gs) = S.addPermanent bears S.alice g1
        request = (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])) {Request.question = Question.MkQuestion S.alice (Text.pack "DeclareAttackers") (R.Array [PromptOffer.object Protocol.namer one, PromptOffer.object Protocol.namer two]) Nothing}
        tap picked label = let view = Question.Web.view request Question.Web.blank {Question.Web.selected = picked} in Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
    Spec.assertEqWith s "first" (tap Set.empty "Your Grizzly Bears 2/2 \x00d7\&2") (Just (Question.Web.MkToggle 0 0))
    Spec.assertEqWith s "then the next" (tap (Set.singleton (0, 0)) "1 of Your Grizzly Bears 2/2 \x00d7\&2") (Just (Question.Web.MkToggle 0 1))
    Spec.assertBool s (Text.pack "aria-keyshortcuts=\"A\">All</button>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "A attacks with all"

  Spec.it s "the log heads each turn the seat reads into, once" $ do
    let view = PlayerView.viewFor S.alice (Setup.emptyGame S.bothPlayers)
        (marks, first) = Log.unseen Map.empty view
    Spec.assertEqWith s "heading" (take 1 first) [Log.MkTurn (PlayerView.Type.turn view) (PlayerView.Type.active view)]
    Spec.assertEqWith s "read" (snd (Log.unseen marks view)) []
    let event = Log.MkEvent Log.MkMajor . Text.pack
        turn n = Log.MkTurn n S.alice
    let busy = Question.Web.blank {Question.Web.logged = Seq.fromList (fmap (event . show) [1 :: Int .. 6]), Question.Web.fresh = 6}
        asked = requestFor (Setup.emptyGame S.bothPlayers) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
    Spec.assertBool s (Text.pack "\x2026\&and 2 more in the log" `Text.isInfixOf` Html.content (Question.Web.view asked busy)) "news says how many more"
    let shock = ObjectId.MkObjectId 5
        linked = Log.item S.alice (Just (Map.fromList [(Text.pack "Shock", shock), (Text.pack "Shocking Grasp", ObjectId.MkObjectId 6)], Question.Web.MkRead)) False (event "Player 1 casts Shocking Grasp, then Shock.")
        tapped label = Html.labelled (Text.pack "button") (Text.pack label) linked >>= \path -> Html.handler (Text.pack "click") path linked
    Spec.assertEqWith s "a named card opens its sheet" (tapped "Shock") (Just (Question.Web.MkRead shock))
    Spec.assertEqWith s "the longest name wins" (tapped "Shocking Grasp") (Just (Question.Web.MkRead (ObjectId.MkObjectId 6)))
    Spec.assertEqWith s "newest turn first, its heading on top" (fmap snd (Foldable.toList (Log.newestFirst (Seq.fromList (zip [0 :: Int ..] [event "a", turn 1, event "b", event "c", turn 2, event "d"]))))) [turn 2, event "d", turn 1, event "c", event "b", event "a"]

  Spec.it s "a priority offering only a pass, the hotseat's concession, and play or draw are answered unasked" $ do
    let gs = Setup.emptyGame S.bothPlayers
    Spec.assertEqWith s "pass" (Question.Web.automatic Question.Web.MkStandard (requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass]))) (Just (R.Number 0))
    Spec.assertEqWith s "concede" (Question.Web.automatic Question.Web.MkStandard (bare (Text.pack "Concede") R.Null)) (Just (R.Text (Text.pack "Continues")))
    Spec.assertEqWith s "play first" (Question.Web.automatic Question.Web.MkStandard (bare (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]))) (Just (R.Text (Text.pack "Play")))

  Spec.it s "full control asks a lone pass and who plays first; auto passes an empty stack outside the seat's main phases" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs0) = S.addObjectIn Zone.Hand mountain S.alice (Setup.emptyGame S.bothPlayers)
        at phase = gs0 {GameState.turn = (GameState.turn gs0) {TurnRegisters.phase = phase}}
        offering phase = requestFor (at phase) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass, Action.Play land Nothing])
        lonePass = requestFor gs0 (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
    Spec.assertEqWith s "full: a lone pass" (Question.Web.automatic Question.Web.MkFull lonePass) Nothing
    Spec.assertEqWith s "full: play first" (Question.Web.automatic Question.Web.MkFull (bare (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]))) Nothing
    Spec.assertEqWith s "auto: upkeep" (Question.Web.automatic Question.Web.MkAutoPass (offering (Phase.Beginning BeginningStep.Upkeep))) (Just (R.Number 0))
    Spec.assertEqWith s "auto: main phase" (Question.Web.automatic Question.Web.MkAutoPass (offering Phase.PrecombatMain)) Nothing
    Spec.assertEqWith s "standard: upkeep" (Question.Web.automatic Question.Web.MkStandard (offering (Phase.Beginning BeginningStep.Upkeep))) Nothing
    Spec.assertEqWith s "a covered cost is paid" (Question.Web.paying (bare (Text.pack "ChooseExtraManaSource") R.Null)) (Just R.Null)
    Spec.assertEqWith s "named" (Question.Web.levelNamed "auto") (Just Question.Web.MkAutoPass)

  Spec.it s "a payment shows each source with what it makes, and a tap answers with that source" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (land, gs) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
        source = R.Object [(Text.pack "source", PromptOffer.object Protocol.namer land), (Text.pack "makes", R.Array [R.Array [Either.fromRight R.Null (Codec.Reply.fromValue (Codec.encode Codec.ManaType.codec (ManaType.Colored Color.Red)))]])]
        shown = requestFor gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])
        request = shown {Request.question = Question.MkQuestion S.alice (Text.pack "ChooseManaSource") (R.Object [(Text.pack "sources", R.Array [source])]) Nothing}
        page = Question.Web.view request Question.Web.blank
    Spec.assertBool s (Text.pack "<h2>Choose a source.</h2>" `Text.isInfixOf` Html.toText page) "heading"
    Spec.assertEqWith s "tap" (fmap (\event -> Question.Web.step request Set.empty event Text.empty) (Html.labelled (Text.pack "button") (Text.pack "Your Mountain (makes R)") page >>= \path -> Html.handler (Text.pack "click") path page)) (Just (Question.Web.MkReplied (PromptOffer.object Protocol.namer land)))
    -- The Mountain's tile on the battlefield pays too.
    Spec.assertEqWith s "tile" (Html.search isCard page >>= \path -> Html.handler (Text.pack "click") path page) (Just (Question.Web.MkAnswer (PromptOffer.object Protocol.namer land)))

  Spec.it s "a bottom pick toggles cards and answers with exactly the count picked" $ do
    let request = asking (Text.pack "Bottom") (R.Object [(Text.pack "from", R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]), (Text.pack "count", R.Number 1)])
        after = picking request
    Spec.assertEqWith s "too few" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose 1 card to put on the bottom of your library: pick 1."))
    Spec.assertEqWith s "picked" (Question.Web.step request (after [Question.Web.MkToggle 0 1]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Text (Text.pack "$2")]))
    Spec.assertEqWith s "unpicked" (after [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 1]) Set.empty

  Spec.it s "CR 508.1a attackers are any of those offered, none included" $ do
    let request = asking (Text.pack "DeclareAttackers") (R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")])
    Spec.assertEqWith s "none" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array []))
    Spec.assertEqWith s "both" (Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]))

  Spec.it s "CR 509.1a blocks pair each blocker picked for with the attackers it blocks" $ do
    let request = asking (Text.pack "DeclareBlockers") (R.Object [(Text.pack "blockers", R.Array [R.Text (Text.pack "$3"), R.Text (Text.pack "$4")]), (Text.pack "attackers", R.Array [R.Text (Text.pack "$1")])])
    Spec.assertEqWith s "one block" (Question.Web.step request (picking request [Question.Web.MkToggle 1 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [R.Text (Text.pack "$4"), R.Array [R.Text (Text.pack "$1")]]]))
    -- On the table, a blocker's tile is tapped first, then the attacker's.
    Spec.assertEqWith s "table" (Question.Web.blocks request) (Just ([ObjectId.MkObjectId 3, ObjectId.MkObjectId 4], [ObjectId.MkObjectId 1]))
    Spec.assertEqWith s "focus keeps picks" (Question.Web.step request (Set.singleton (1, 0)) (Question.Web.MkFocus 0) Text.empty) (Question.Web.MkPicking (Set.singleton (1, 0)))

  Spec.it s "CR 601.2c targets are exactly each slot's count, sent by slot" $ do
    let slot = R.Text (Text.pack "target")
        request = asking (Text.pack "ChooseTargets") (R.Object [(Text.pack "source", R.Text (Text.pack "$9")), (Text.pack "slots", R.Array [R.Array [slot, R.Array [R.Number 1, R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]]]])])
    Spec.assertEqWith s "too few" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkRefused (Text.pack "Choose 1 target for object 9 (target): pick 1."))
    Spec.assertEqWith s "one" (Question.Web.step request (picking request [Question.Web.MkToggle 0 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [slot, R.Array [R.Text (Text.pack "$1")]]]))

  Spec.it s "CR 603.3b triggered abilities are ordered by tapping each in turn, answered by Done" $ do
    let request = asking (Text.pack "OrderTriggers") (R.Array [R.Object [(Text.pack "source", R.Text (Text.pack "$1"))], R.Object [(Text.pack "source", R.Text (Text.pack "$2"))], R.Object [(Text.pack "source", R.Text (Text.pack "$3"))]])
    Spec.assertEqWith s "first" (Question.Web.step request Set.empty (Question.Web.MkToggle 0 1) Text.empty) (Question.Web.MkPicking (Set.singleton (0, 1)))
    Spec.assertEqWith s "the last waits for Done" (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 2, Question.Web.MkToggle 0 0]) (Set.fromList [(0, 1), (1, 2), (2, 0)])
    Spec.assertEqWith s "done" (Question.Web.step request (Set.fromList [(0, 1), (1, 2), (2, 0)]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Number 1, R.Number 2, R.Number 0]))
    Spec.assertEqWith s "a placed item is taken out, those after it moving up" (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 2, Question.Web.MkToggle 0 1]) (Set.singleton (0, 2))
    Spec.assertEqWith s "the rest follow in the order offered" (Question.Web.step request (Set.singleton (0, 2)) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Number 2, R.Number 0, R.Number 1]))

  Spec.it s "CR 510.1c combat damage goes to a lone recipient unasked, and among several adds up to the total" $ do
    let offer recipients = R.Object [(Text.pack "source", R.Text (Text.pack "$9")), (Text.pack "lethal", R.Array [R.Array [r, R.Number 1] | r <- recipients]), (Text.pack "paid", R.Array []), (Text.pack "total", R.Number 3)]
        one = R.Text (Text.pack "$1")
        two = R.Text (Text.pack "$2")
        request = asking (Text.pack "AssignCombatDamage") (offer [one, two])
    Spec.assertEqWith s "lone" (Question.Web.automatic Question.Web.MkStandard (asking (Text.pack "AssignCombatDamage") (offer [one]))) (Just (R.Array [R.Array [one, R.Number 3]]))
    Spec.assertEqWith s "several" (Question.Web.automatic Question.Web.MkStandard request) Nothing
    Spec.assertEqWith s "adds up" (Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 2]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one, R.Number 1], R.Array [two, R.Number 2]]))
    Spec.assertBool s (case Question.Web.step request (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 1]) Question.Web.MkSubmit Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "short"
    Spec.assertEqWith s "a pick replaces the row's last" (picking request [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 3]) (Set.singleton (0, 3))
    -- The form starts lethal to the first and the rest to the last, which answers as it stands.
    Spec.assertEqWith s "default" (Question.Web.initial request) (Set.fromList [(0, 1), (1, 2)])
    Spec.assertEqWith s "default answers" (Question.Web.step request (Question.Web.initial request) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one, R.Number 1], R.Array [two, R.Number 2]]))

  Spec.it s "CR 701.23b a search finds up to its count, none included" $ do
    let request = asking (Text.pack "Search") (R.Object [(Text.pack "from", R.Array [R.Text (Text.pack "$1"), R.Text (Text.pack "$2")]), (Text.pack "names", R.Array [R.Text (Text.pack "Forest"), R.Text (Text.pack "Plains")]), (Text.pack "count", R.Number 1)])
    Spec.assertBool s (Text.pack "<button type=\"button\">Forest</button><button type=\"button\">Plains</button><button class=\"primary\" type=\"button\">Find</button>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "names"
    Spec.assertEqWith s "none" (Question.Web.step request Set.empty Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array []))
    Spec.assertBool s (case Question.Web.step request (picking request [Question.Web.MkToggle 0 0, Question.Web.MkToggle 0 1]) Question.Web.MkSubmit Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "too many"

  Spec.it s "CR 701.22a a scry places each card on top or bottom in the order tapped, sent as the bottom pile then the top" $ do
    let one = R.Text (Text.pack "$1")
        two = R.Text (Text.pack "$2")
        request = asking (Text.pack "ChooseScry") (R.Object [(Text.pack "from", R.Array [one, two])])
    Spec.assertEqWith s "placed" (Question.Web.step request (picking request [Question.Web.MkToggle 1 0, Question.Web.MkToggle 0 1]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [one], R.Array [two]]))
    Spec.assertEqWith s "the last waits for Done" (picking request [Question.Web.MkToggle 1 0, Question.Web.MkToggle 0 1]) (Set.fromList [(1, 0), (2, 1)])
    Spec.assertEqWith s "a placed card is taken out" (picking request [Question.Web.MkToggle 1 0, Question.Web.MkToggle 0 1, Question.Web.MkToggle 1 0]) (Set.singleton (0, 1))
    Spec.assertEqWith s "the untapped stay on top below those placed there" (Question.Web.step request (Set.singleton (1, 1)) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Array [two], R.Array [one]]))

  Spec.it s "an offer listing an order is ordered by tapping, an offer listing options is one tap, and a range is a number" $ do
    let ordered = asking (Text.pack "OrderTimestamps") (R.Object [(Text.pack "order", R.Array [R.Text (Text.pack "a"), R.Text (Text.pack "b")])])
        listed = asking (Text.pack "ChooseWhatever") (R.Object [(Text.pack "options", R.Array [R.Text (Text.pack "Up"), R.Text (Text.pack "Down")])])
        ranged = asking (Text.pack "ChooseNumber") (R.Object [(Text.pack "least", R.Number 1), (Text.pack "greatest", R.Number 3)])
    Spec.assertEqWith s "ordered" (Question.Web.step ordered (picking ordered [Question.Web.MkToggle 0 1, Question.Web.MkToggle 0 0]) Question.Web.MkSubmit Text.empty) (Question.Web.MkReplied (R.Array [R.Number 1, R.Number 0]))
    Spec.assertBool s (Text.pack "<button type=\"button\">Up</button><button type=\"button\">Down</button>" `Text.isInfixOf` Html.toText (Question.Web.view listed Question.Web.blank)) "listed"
    Spec.assertBool s (Text.pack "<button type=\"button\">1</button><button type=\"button\">2</button><button type=\"button\">3</button>" `Text.isInfixOf` Html.toText (Question.Web.view ranged Question.Web.blank)) "ranged"

  Spec.it s "CR 701.20e library cards looked at are listed, linked and read by name while arranged" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    mountain <- S.printingOf s registry "Mountain"
    let (one, g1) = S.addObjectIn Zone.Library bears S.alice (Setup.emptyGame S.bothPlayers)
        (two, g2) = S.addObjectIn Zone.Library mountain S.alice g1
        look oid = Event.recordEvent (GameEvent.Looked (Looked.MkLooked oid (Projection.project oid g2) (Set.singleton S.alice)))
        request = requestFor (look two (look one g2)) (Prompt.ArrangeLibraryCards (Decider.MkDecider S.alice) S.alice [one, two])
        tap picked label = let view = Question.Web.view request Question.Web.blank {Question.Web.selected = picked} in Html.labelled (Text.pack "button") (Text.pack label) view >>= \path -> Html.handler (Text.pack "click") path view
        reading = Question.Web.view request Question.Web.blank {Question.Web.reading = Just (Card.MkReading one [] True)}
    Spec.assertEqWith s "listed by name" (tap Set.empty "Mountain") (Just (Question.Web.MkToggle 0 1))
    Spec.assertEqWith s "placed, then Done" (tap (Set.singleton (0, 1)) "Done") (Just Question.Web.MkSubmit)
    Spec.assertEqWith s "linked" (fmap (Map.lookup (Text.pack "Grizzly Bears") . Log.linkable) (Request.view request)) (Just (Just one))
    Spec.assertBool s (maybe False (Text.isInfixOf (Text.pack "Grizzly Bears") . Html.toText) (Html.search (\html -> Text.pack "<section class=\"sheet" `Text.isPrefixOf` Html.toText html) reading >>= \path -> Html.at path reading)) "read"

  Spec.it s "a card name is typed as itself" $ do
    let request = asking (Text.pack "ChooseCardName") R.Null
    Spec.assertEqWith s "named" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack " Grizzly Bears ")) (Question.Web.MkReplied (R.Text (Text.pack "Grizzly Bears")))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped Text.empty of Question.Web.MkRefused _ -> True; _ -> False) "empty"

  Spec.it s "any other question shows its offer and reads a reply typed as JSON" $ do
    let request = bare (Text.pack "ChooseX") (R.Object [(Text.pack "least", R.Number 0)])
    Spec.assertBool s (Text.pack "<pre>{&quot;least&quot;:0}</pre>" `Text.isInfixOf` Html.toText (Question.Web.view request Question.Web.blank)) "offer"
    Spec.assertEqWith s "read" (Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "3")) (Question.Web.MkReplied (R.Number 3))
    Spec.assertBool s (case Question.Web.step request Set.empty Question.Web.MkTyped (Text.pack "{") of Question.Web.MkRefused _ -> True; _ -> False) "refused"

-- | Whether a node is a card's tile.
isCard :: Html.Html msg -> Bool
isCard html = Text.pack "<button class=\"card " `Text.isPrefixOf` Html.toText html

-- | The request the line protocol sends for a prompt, with the deciding seat's view.
requestFor :: GameState.GameState -> Prompt.Prompt r -> Request.Request
requestFor gs prompt = case Protocol.questionOf gs prompt of
  Just question -> Request.MkRequest (Just (PlayerView.viewFor (Question.seat question) gs)) question Nothing 0
  Nothing -> bare (Text.pack "unasked") R.Null

-- | A request with no view, for questions whose form reads only the offer.
bare :: Text.Text -> R.Reply -> Request.Request
bare kind offer = Request.MkRequest Nothing (Question.MkQuestion S.alice kind offer Nothing) Nothing 0

-- | A request asking the kind with the offer, with a view of an empty game.
asking :: Text.Text -> R.Reply -> Request.Request
asking kind offer = (requestFor (Setup.emptyGame S.bothPlayers) (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Pass])) {Request.question = Question.MkQuestion S.alice kind offer Nothing}

-- | Every kind of question the scenario corpus answers has a form of its own,
-- none falling back to a reply typed as JSON or labelling an option in JSON,
-- but Concede, which the hotseat
-- answers itself: each kind's first scenario, replayed to the stops about its
-- first answer to the kind (a stop counts entries, not prompts, so a few
-- either side).
corpusSpec :: Spec.Spec IO n -> Registry.Registry IO -> n ()
corpusSpec s registry = Spec.describe s "WebQuestion" . Spec.it s "every kind of question the scenarios answer has a form" $ do
  root <- Load.defaultRoot
  paths <- Load.listRoot root
  firsts <-
    Monad.foldM
      ( \found path -> do
          contents <- Encoding.decodeUtf8Lenient <$> ByteString.readFile path
          pure (foldr (\kind -> Map.insertWith (\_ old -> old) kind path) found (kindsIn contents))
      )
      Map.empty
      paths
  found <- fmap concat . Monad.forM (Map.toList firsts) $ \(kind, path) -> do
    bytes <- ByteString.readFile path
    case (Load.parse bytes, Common.parse (Encoding.decodeUtf8Lenient bytes) >>= Codec.Reply.fromValue) of
      (Right scenario, Right raw) -> do
        let entries = Maybe.fromMaybe [] (Protocol.field "timeline" raw >>= Protocol.arrayOf)
            answering entry = (Protocol.field "do" entry >>= Protocol.field "Answer" >>= Protocol.field "prompt") == Just (R.Text kind)
            at = maybe 0 Int.toNaturalSaturating (List.findIndex answering entries)
        stops <- Monad.forM [Natural.minusSaturating at 2 .. at + 3] $ \i -> Scenario.replay registry (Just i) scenario
        pure
          [ (Question.kind (Request.question request), path, if Question.Web.answersTyped request then Text.pack "typed as JSON" else Text.take 160 (snd (Text.breakOn (Text.pack "{\"") (Html.content (Question.Web.view request Question.Web.blank)))))
          | Right (Stop.MkBefore _ _ asked) <- stops,
            Just request <- [Protocol.requestFor asked],
            Question.kind (Request.question request) /= Text.pack "Concede",
            Question.Web.answersTyped request || Text.isInfixOf (Text.pack "{\"") (Html.content (Question.Web.view request Question.Web.blank))
          ]
      _ -> pure []
  Spec.assertEqWith s "typed" (ListUtils.nubOrd found) []

-- | CR 730.1a: a question a subgame raises says so, in its request and on
-- the turn bar; the main game's does not.
subgameSpec :: Spec.Spec IO n -> Registry.Registry IO -> n ()
subgameSpec s registry = Spec.describe s "WebQuestion" . Spec.it s "CR 730.1a a subgame's question is marked as one" $ do
  root <- Load.defaultRoot
  bytes <- ByteString.readFile (root <> "/game/cr-730-1b-730-3-gameplay-alice-casts-a-subgame-spell-bob-decks-bob-loses-3.json")
  case Load.parse bytes of
    Left problem -> Spec.assertFailure s (Text.unpack problem)
    Right scenario -> do
      let at i = do
            stop <- Scenario.replay registry (Just i) scenario
            pure $ case stop of
              Right (Stop.MkBefore _ _ asked) -> Protocol.requestFor asked
              _ -> Nothing
      before <- at 0
      -- Entry 5 keeps a hand in the subgame (CR 730.2).
      inside <- at 5
      Spec.assertEqWith s "the main game" (fmap Request.depth before) (Just 0)
      Spec.assertEqWith s "the subgame" (fmap Request.depth inside) (Just 1)
      Spec.assertBool s (maybe False (Text.isInfixOf (Text.pack "class=\"subgame\"") . Html.toText . (`Question.Web.view` Question.Web.blank)) inside) "the turn bar says so"

-- | The replay page (Parun.Web.Replay) at scenario positions: a subgame's
-- (CR 730.1a), a turn's extra phase (CR 500.8) and a skipped one (CR
-- 500.11) on the turn bar, and a card on an adventure in exile (CR 716.3d).
replaySpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
replaySpec s registry = Spec.describe s "WebQuestion" . Spec.describe s "replay" $ do
  Spec.it s "CR 730.1a a replayed subgame position is marked as one" $ do
    let named = "game/cr-730-1b-730-3-gameplay-alice-casts-a-subgame-spell-bob-decks-bob-loses-3"
    before <- replayed s registry named 0
    -- Entry 5 keeps a hand in the subgame (CR 730.2).
    inside <- replayed s registry named 5
    let marked stop = Text.isInfixOf (Text.pack "class=\"subgame\"") (Html.toText (Replay.view (frameAt stop)))
    Spec.assertBool s (not (marked before)) "the main game is not"
    Spec.assertBool s (marked inside) "the subgame is"

  Spec.it s "CR 500.8 an added beginning phase is lit as extra, the phase before it passed" $ do
    -- Entry 11 is answered in the upkeep Sphinx of the Second Sun adds after
    -- the first of the turn's two postcombat main phases, Relentless
    -- Assault's added combat and main phase having run; the turn's own combat
    -- and postcombat main phase are still to come.
    stop <- replayed s registry "sphinx-of-the-second-sun-adds-a-beginning-phase-that-neither-settles-nor-grants-a-land" 10
    let bar = turnBarOf stop
    Spec.assertEqWith s "the step" (PlayerView.Type.phase (boardOf stop)) (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertBool s (Text.isInfixOf (Text.pack "<li class=\"now extra\"") bar) ("lit as extra: " <> Text.unpack bar)
    Spec.assertBool s (Text.isInfixOf (Text.pack "<li class=\"past\">Main</li>") bar) "the precombat main phase passed"
    Spec.assertEqWith s "still to come" [label | label <- ["Combat", "Main 2", "End"], Text.isInfixOf (Text.pack ("<li>" <> label <> "</li>")) bar] ["Combat", "Main 2", "End"]
    Spec.assertBool s (Text.isInfixOf (Text.pack "<span class=\"extra\"") bar) "said beside the turn"

  Spec.it s "CR 500.11 a skipped combat phase shows as passed and nothing as extra" $ do
    -- Entry 3 is checked in bob's postcombat main phase, his combat skipped.
    stop <- replayed s registry "blinding-angel-makes-the-player-it-hits-skip-their-next-combat" 2
    let bar = turnBarOf stop
    Spec.assertEqWith s "the step" (PlayerView.Type.phase (boardOf stop)) Phase.PostcombatMain
    Spec.assertBool s (Text.isInfixOf (Text.pack "<li class=\"past\">Combat</li>") bar) ("combat passed: " <> Text.unpack bar)
    Spec.assertBool s (Text.isInfixOf (Text.pack "<li class=\"now\">Main 2</li>") bar) "the main phase lit"
    Spec.assertBool s (not (Text.isInfixOf (Text.pack "extra") bar)) "nothing extra"

  Spec.it s "CR 716.3d a card exiled as its Adventure resolved is marked on its tile and sheet" $ do
    -- Entry 10 follows Can't Wake Up's resolution.
    after <- replayed s registry "chancellor-of-tales-copies-an-adventure-onto-a-new-target" 9
    -- Flaxen Intruder put into exile by the scenario, never cast.
    placed <- replayed s registry "howling-galefang-has-haste-while-you-own-an-exiled-adventure-card--control" 0
    let exiled name stop = [(o, b) | let b = boardOf stop, seat <- PlayerView.Type.seats b, o <- SeatView.exile seat, Render.nameOf b o == Text.pack name]
        marks = Board.MkMarks (\o -> Card.MkCue [] (ObjectView.object o) Nothing) (const Nothing)
        said = Text.isInfixOf (Text.pack "On an adventure")
    case (exiled "Cruel Somnophage" after, exiled "Flaxen Intruder" placed) of
      ([(o, b)], [(other, b')]) -> do
        Spec.assertBool s (said (Html.toText (Board.view marks b))) "its tile"
        Spec.assertBool s (said (Html.toText (Card.sheet b o (Card.MkReading (ObjectView.object o) [] True) [] (ObjectView.object o)))) "its sheet"
        Spec.assertBool s (not (said (Html.toText (Board.view marks b')) || ObjectView.onAdventure other)) "not a card merely exiled"
      (one, two) -> Spec.assertFailure s ("expected one exiled card in each: " <> show (length one, length two))

  Spec.it s "CR 612.2 a Sleight of Mind's swap shows on the Paladin's sheet, and the Clone of it reads as printed" $ do
    -- Entry 4 casts the Clone after the swap resolves; entry 9 follows the
    -- Terror's resolution, the Paladin in the graveyard and the Clone left.
    swapped <- replayed s registry "text-change/cr-612-2-a-sleight-of-mind-turns-protection-from-black-into-green-but-a-clone-keeps-it" 3
    copied <- replayed s registry "text-change/cr-612-2-a-sleight-of-mind-turns-protection-from-black-into-green-but-a-clone-keeps-it" 8
    paladin <- sheetText s registry swapped "Paladin en-Vec"
    clone <- sheetText s registry copied "Paladin en-Vec"
    Spec.assertEqWith s "the Paladin's abilities now" paladin [Text.pack "First strike", Text.pack "Protection from red and from green"]
    Spec.assertEqWith s "the Clone's, as printed" (fmap (Text.takeWhile (/= '(')) clone) [Text.pack "First strike, protection from black and from red "]

  Spec.it s "CR 613.1f Humility's wipe shows as no abilities, on itself and on History of Benalia under Opalescence" $ do
    -- Entry 3 follows History of Benalia's entry.
    stop <- replayed s registry "saga/cr-613-1f-history-of-benalia-under-opalescence-and-humility-enters-with-no-lore" 2
    humility <- sheetText s registry stop "Humility"
    history <- sheetText s registry stop "History of Benalia"
    plains <- sheetText s registry stop "Plains"
    opalescence <- sheetText s registry stop "Opalescence"
    Spec.assertEqWith s "Humility" humility [Text.pack "It has no abilities."]
    Spec.assertEqWith s "History of Benalia" history [Text.pack "It has no abilities."]
    Spec.assertEqWith s "a Plains, as printed" plains [Text.pack "({T}: Add {W}.)"]
    Spec.assertBool s (not (null opalescence) && notElem (Text.pack "It has no abilities.") opalescence) ("Opalescence, as printed: " <> show opalescence)

  Spec.it s "CR 305.7 a Palace Guard stripped by Blood Moon is marked as changed, its block permission unworded" $ do
    stripped <- replayed s registry "combat-cost/cr-305-7-an-animated-palace-guard-set-to-mountain-blocks-stripped" 0
    control <- replayed s registry "combat-cost/cr-305-7-an-animated-palace-guard-set-to-mountain-blocks-control" 0
    guard <- sheetText s registry stripped "Palace Guard"
    unchanged <- sheetText s registry control "Palace Guard"
    Spec.assertEqWith s "stripped" (take 1 guard) [Text.pack "Its abilities have changed; this text may be out of date."]
    Spec.assertEqWith s "the control, as printed" unchanged [Text.pack "This creature can block any number of creatures."]

  Spec.it s "CR 702.140e a mutated permanent's sheet reads its top card as printed and then the card merged under it" $ do
    -- Entry 9 follows Cubwarden's mutate over Silent Arbiter.
    stop <- replayed s registry "mutate/cr-702-140e-a-silent-arbiter-under-a-cubwarden-still-holds-alice-to-one-attacker" 8
    cubwarden <- S.printingOf s registry "Cubwarden"
    arbiter <- S.printingOf s registry "Silent Arbiter"
    sheet <- sheetText s registry stop "Cubwarden"
    let printedOf = foldMap Text.lines . Face.oracleText . NonEmpty.head . Card.Type.faces . Printing.card
    Spec.assertEqWith s "the sheet" sheet ([Text.pack "Cubwarden:"] <> printedOf cubwarden <> [Text.pack "Silent Arbiter (merged under it):"] <> printedOf arbiter)

  Spec.it s "CR 731.2e / 708.5 a face-down card merged under a face-up one is named to its controller alone" $ do
    -- Entry 7 follows Cubwarden's mutate over a face-down Misthoof Kirin.
    stop <- replayed s registry "mutate/cr-731-2e-a-merge-over-a-face-down-permanent-leaves-the-topmost-component-s-face" 6
    let mergedFor seat = [ObjectView.merged o | o <- PlayerView.Type.battlefield (PlayerView.viewFor seat (Stop.state stop)), not (null (ObjectView.merged o))]
    Spec.assertEqWith s "alice" (mergedFor S.alice) [Seq.singleton (Just (CardName.MkCardName (Text.pack "Misthoof Kirin")))]
    Spec.assertEqWith s "bob" (mergedFor S.bob) [Seq.singleton Nothing]
    sheet <- sheetText s registry stop "Cubwarden"
    Spec.assertBool s (Text.pack "Misthoof Kirin (merged under it):" `elem` sheet) ("alice's sheet: " <> show sheet)

  Spec.it s "CR 731.2f a face-down merged permanent's controller reads its top card as printed and the card under it" $ do
    -- Entry 9 follows Cyber Conversion turning Cubwarden over a manifested
    -- Thragtusk face down.
    stop <- replayed s registry "mutate/cr-731-2g-a-face-down-merged-permanent-with-a-sorcery-card-in-it-stays-face-down--control" 8
    cubwarden <- S.printingOf s registry "Cubwarden"
    thragtusk <- S.printingOf s registry "Thragtusk"
    sheet <- sheetWhere s registry stop "the face-down pile" (\_ o -> not (null (ObjectView.merged o)))
    let printedOf = foldMap Text.lines . Face.oracleText . NonEmpty.head . Card.Type.faces . Printing.card
    Spec.assertEqWith s "the sheet" sheet ([Text.pack "Cubwarden:"] <> printedOf cubwarden <> [Text.pack "Thragtusk (merged under it):"] <> printedOf thragtusk)
    let mergedFor seat = [ObjectView.merged o | o <- PlayerView.Type.battlefield (PlayerView.viewFor seat (Stop.state stop)), not (null (ObjectView.merged o))]
    Spec.assertEqWith s "bob" (mergedFor S.bob) [Seq.singleton Nothing]

-- | The sheet's Oracle text (Card.oracle) of the one battlefield object named,
-- as the stop's active player reads it.
sheetText :: Spec.Spec IO n -> Registry.Registry IO -> Stop.Stop -> String -> IO [Text.Text]
sheetText s registry stop name = sheetWhere s registry stop name (\board o -> Render.nameOf board o == Text.pack name)

-- | sheetText of the first battlefield object the predicate picks, described.
sheetWhere :: Spec.Spec IO n -> Registry.Registry IO -> Stop.Stop -> String -> (PlayerView.Type.PlayerView -> ObjectView.ObjectView -> Bool) -> IO [Text.Text]
sheetWhere s registry stop name picks = do
  let board = boardOf stop
  case filter (picks board) (PlayerView.Type.battlefield board) of
    o : _ -> do
      looked <- Monad.forM (Set.toList (Render.cardNames o) <> Maybe.catMaybes (Foldable.toList (ObjectView.merged o))) $ \n -> fmap ((,) n) (Registry.named registry (Text.unpack (CardName.unwrap n)))
      let lookUp n = Maybe.fromMaybe (Right Nothing) (lookup n looked)
      either (Spec.assertFailure s . show) pure (Card.oracle lookUp o)
    [] -> Spec.assertFailure s ("no " <> name <> " on the battlefield")

-- | The scenario named (beneath data/scenarios, less its extension) replayed
-- to a position.
replayed :: Spec.Spec IO n -> Registry.Registry IO -> String -> Natural -> IO Stop.Stop
replayed s registry name at = do
  root <- Load.defaultRoot
  bytes <- ByteString.readFile (root <> "/" <> name <> ".json")
  case Load.parse bytes of
    Left problem -> Spec.assertFailure s (Text.unpack problem)
    Right scenario -> do
      stopped <- Scenario.replay registry (Just at) scenario
      either (Spec.assertFailure s . Text.unpack . Scenario.render) pure stopped

-- | The stop's board as its active player sees it.
boardOf :: Stop.Stop -> PlayerView.Type.PlayerView
boardOf stop = let gs = Stop.state stop in PlayerView.viewFor (TurnRegisters.activePlayer (GameState.turn gs)) gs

-- | The replay page's frame of a stop, as Parun.Browser.Replay.frameOf
-- builds it.
frameAt :: Stop.Stop -> Replay.Frame
frameAt stop =
  Replay.MkFrame
    { Replay.name = Text.pack "a-scenario",
      Replay.description = Text.empty,
      Replay.seats = [],
      Replay.viewer = PlayerView.Type.viewer (boardOf stop),
      Replay.position = 0,
      Replay.positions = 0,
      Replay.board = Just (boardOf stop),
      Replay.depth = case stop of
        Stop.MkBefore _ _ asked -> Protocol.depthOf asked
        Stop.MkEnded _ -> 0,
      Replay.entry = Nothing,
      Replay.question = Nothing,
      Replay.failure = Nothing,
      Replay.reading = Nothing
    }

-- | The turn bar of the replay page at the stop.
turnBarOf :: Stop.Stop -> Text.Text
turnBarOf stop = fst (Text.breakOn (Text.pack "<div class=\"play-area\"") (Html.toText (Replay.view (frameAt stop))))

-- | The prompt kinds a scenario's text names in its answers.
kindsIn :: Text.Text -> [Text.Text]
kindsIn contents =
  [ Text.takeWhile (/= '"') (Text.drop 1 (Text.stripStart (Text.drop 1 (Text.stripStart rest))))
  | (_, found) <- Text.breakOnAll (Text.pack "\"prompt\"") contents,
    let rest = Text.drop 8 found
  ]

-- | What submitting the picks answers, as the engine reads it and its offer
-- lets it stand.
answered :: Prompt.Prompt r -> Request.Request -> Set.Set (Natural, Natural) -> Either Text.Text r
answered prompt request picks = case Question.Web.step request picks Question.Web.MkSubmit Text.empty of
  Question.Web.MkReplied reply -> Protocol.decodeAnswer prompt reply >>= Protocol.onOffer (Question.offer (Request.question request)) prompt
  other -> Left (Text.pack (show other))

-- | The picks a run of events leaves.
picking :: Request.Request -> [Question.Web.Event] -> Set.Set (Natural, Natural)
picking request = Foldable.foldl' (\picked event -> case Question.Web.step request picked event Text.empty of Question.Web.MkPicking now -> now; _ -> picked) Set.empty
