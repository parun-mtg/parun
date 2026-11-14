{-# LANGUAGE GADTs #-}

-- Covers Parun.Play.Protocol: a seat's decisions as lines out and answers back,
-- and that nothing a seat may not see reaches its lines.
module Parun.ProtocolSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Cards as Cards
import qualified Parun.Codec.Cost as Codec.Cost
import qualified Parun.Codec.CostComponent as Codec.CostComponent
import qualified Parun.Codec.EntryOption as Codec.EntryOption
import qualified Parun.Codec.Filter as Codec.Filter
import qualified Parun.Codec.GameEvent as Codec.GameEvent
import qualified Parun.Codec.Keyword as Codec.Keyword
import qualified Parun.Codec.Reply as Codec.Reply
import qualified Parun.Codec.Request as Codec.Request
import qualified Parun.Codec.SlotName as Codec.SlotName
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Binding as Binding
import qualified Parun.Engine.Combat as Combat
import qualified Parun.Engine.Driver as Driver
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Engine.Resolve.Effect as Resolve
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Subtype as Engine.Subtype
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Play.Protocol as Protocol
import qualified Parun.Registry as Registry
import qualified Parun.Scenario as Scenario
import qualified Parun.Scenario.Offer as Offer
import qualified Parun.Scenario.Prompt as Scenario.Prompt
import qualified Parun.Scenario.Reply as Reply
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.Action as Action
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.AnyNumberOf as AnyNumberOf
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.CandidateCost as CandidateCost
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.ChoosePawprints as ChoosePawprints
import qualified Parun.Types.ClauseIndex as ClauseIndex
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Combat as Combat.Type
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterSpread as CounterSpread
import qualified Parun.Types.DamageEvent as DamageEvent
import qualified Parun.Types.DamageKind as DamageKind
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Deck as Deck
import qualified Parun.Types.EntryOption as EntryOption
import qualified Parun.Types.EntryR as EntryR
import qualified Parun.Types.EntryRewrite as EntryRewrite
import qualified Parun.Types.Face as Face
import qualified Parun.Types.FaceDownReason as FaceDownReason
import qualified Parun.Types.FaceDownState as FaceDownState
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Filter as Filter.Type
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GraveyardArrangement as GraveyardArrangement
import qualified Parun.Types.HandActionIndex as HandActionIndex
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.KickerDecision as KickerDecision
import qualified Parun.Types.LandOrNonland as LandOrNonland
import qualified Parun.Types.LearnMode as LearnMode
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.Looked as Looked
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Measure as Measure
import qualified Parun.Types.Measures as Measures
import qualified Parun.Types.ModeIndex as ModeIndex
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.MoveSpread as MoveSpread
import qualified Parun.Types.MutateSide as MutateSide
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Observer as Observer
import qualified Parun.Types.Operand as Operand
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.OutsideCard as OutsideCard
import qualified Parun.Types.PayOffer as PayOffer
import qualified Parun.Types.PayerAnswer as PayerAnswer
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Question as Question
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.ReplacementEffect as ReplacementEffect
import qualified Parun.Types.ReplacementEntry as ReplacementEntry
import qualified Parun.Types.Reply as R
import qualified Parun.Types.Request as Request
import qualified Parun.Types.RollAdjustment as RollAdjustment
import qualified Parun.Types.SearchPlace as SearchPlace
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.StoredResult as StoredResult
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.SubtypeFamily as SubtypeFamily
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TargetCount as TargetCount
import qualified Parun.Types.TextWord as TextWord
import qualified Parun.Types.ThrowResult as ThrowResult
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.TurnUpProcedure as TurnUpProcedure
import qualified Parun.Types.Undoing as Undoing
import qualified Parun.Types.WordFamily as WordFamily
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Protocol" $ do
  Spec.it s "every answer in ten turns of a game survives the protocol's encode and decode" $ do
    matchup <- redGreen s registry
    let checking :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.State [(Text.Text, Bool)])
        checking = Answerer.MkAnswerer $ \asked -> do
          let prompt = Asked.prompt asked
              answer = S.castAnswer prompt
              back = Protocol.decodeAnswer prompt (Protocol.encodeAnswer prompt answer)
              survives = fmap (Replay.encode prompt) back == Right (Replay.encode prompt answer)
          Maybe.maybe (pure ()) (\_ -> State.modify' (<> [(Scenario.Prompt.kindOf prompt, survives)])) (Driver.deciderOf prompt)
          pure answer
        played = do
          (_, gs) <- Engine.runGameAsked (Answerer.unwrap checking) (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup)
          Driver.drive [] checking 100000 (pure . (>= 10) . (TurnRegisters.turnNumber . GameState.turn)) gs
        checks = State.execState played []
    Spec.assertBool s (Set.fromList (fmap Text.pack ["Concede", "DeclareMulligan", "ChooseAction", "ChooseManaSource", "ChooseDiscard"]) `Set.isSubsetOf` Set.fromList (fmap fst checks)) "reached"
    Spec.assertEqWith s "all survive" [kind | (kind, False) <- checks] []
  Spec.it s "attacks, blocks and targets survive the protocol's encode and decode" $ do
    let survives :: Prompt.Prompt r -> r -> Bool
        survives prompt answer = fmap (Replay.encode prompt) (Protocol.decodeAnswer prompt (Protocol.encodeAnswer prompt answer)) == Right (Replay.encode prompt answer)
        o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        slot = SlotName.MkSlotName (Text.pack "target")
    Spec.assertBool s (survives (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [o7, o9]) [o9]) "attackers"
    Spec.assertBool s (survives (Prompt.DeclareBlockers (Decider.MkDecider S.bob) S.bob [o9] [o7]) (Map.singleton o9 (Set.singleton o7))) "blockers"
    Spec.assertBool s (survives (Prompt.ChooseTargets (Decider.MkDecider S.alice) S.alice o7 (Map.singleton slot (1, Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToCreature o9]))) (Map.singleton slot (Set.singleton (Recipient.ToPlayer S.bob)))) "targets"
  Spec.it s "CR 103.1 a seat on the line is asked to play first or draw before the game, with no view" $ do
    let asking replies = State.runState (Protocol.playOrDraw (\line -> State.state (\(out, rest) -> (Maybe.listToMaybe rest, (out <> [line], drop 1 rest)))) S.bob) ([], fmap Text.pack replies)
        (drew, (sent, _)) = asking ["\"Maybe\"", "\"Draw\""]
        requests = fmap (requestOf . Text.unpack) sent
    Spec.assertEqWith s "draws" drew (Just False)
    Spec.assertEqWith s "plays" (fst (asking ["\"Play\""])) (Just True)
    Spec.assertEqWith s "end of input" (fst (asking [])) Nothing
    Spec.assertEqWith s "views" (fmap (fmap Request.view) requests) [Just Nothing, Just Nothing]
    Spec.assertEqWith s "questions" (fmap (fmap Request.question) requests) (replicate 2 (Just (Question.MkQuestion S.bob (Text.pack "PlayOrDraw") (R.Array [R.Text (Text.pack "Play"), R.Text (Text.pack "Draw")]))))
    Spec.assertEqWith s "problems" (fmap (fmap Request.problem) requests) [Just Nothing, Just (Just (Text.pack "expected Play or Draw"))]
  Spec.it s "an activated ability is offered by its place among its source's, so two abilities of one source differ" $ do
    vane <- S.printingOf s registry "Arcum's Weathervane"
    let (oid, gs) = S.addPermanent vane S.alice (Setup.emptyGame S.bothPlayers)
        offered = Protocol.offerOf gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice (fmap (Action.Activate oid) (Projection.abilitiesOf oid gs)))
        activate i = R.Object [(Text.pack "action", R.Text (Text.pack "Activate")), (Text.pack "object", Protocol.object oid), (Text.pack "ability", R.Number i)]
    Spec.assertEqWith s "offer" offered (R.Array [activate 0, activate 1])
  Spec.it s "a scenario names an offer by its action and object, then only the fields telling it from its action's other offers" $ do
    let offer kind extra = R.Object ((Text.pack "action", R.Text (Text.pack kind)) : [(Text.pack key, value) | (key, value) <- extra])
        on label = ("object", R.Text (Text.pack label))
        named name = ("name", R.Text (Text.pack name))
        offers =
          [ offer "Pass" [],
            offer "Activate" [on "$vane", ("ability", R.Number 0)],
            offer "Activate" [on "$vane", ("ability", R.Number 1)],
            offer "Activate" [on "$tower", ("ability", R.Number 0)],
            offer "Cast" [on "$ainok", named "Ainok Tracker"],
            offer "Cast" [on "$ainok", named "Ainok Tracker", ("faceDown", R.Text (Text.pack "Morphed"))],
            offer "Cast" [on "$giant", named "Bonecrusher Giant"],
            offer "Cast" [on "$giant", named "Stomp"],
            offer "PayLifeForMana" [("life", R.Number 1), ("mana", R.Text (Text.pack "Colorless"))]
          ]
    Spec.assertEqWith
      s
      "lines"
      (Offer.linesOf offers)
      (fmap Text.pack ["Pass", "Activate $vane 0", "Activate $vane 1", "Activate $tower", "Cast $ainok", "Cast $ainok Morphed", "Cast $giant Bonecrusher Giant", "Cast $giant Stomp", "PayLifeForMana"])

  Spec.it s "CR 701.40c / 116.2d / 702.170a a turn-up, ignore or plot is offered with its procedure, ability or cost" $ do
    let oid = ObjectId.MkObjectId 7
        gs = Setup.emptyGame S.bothPlayers
        cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []
        ban = AbilityName.MkAbilityName (Text.pack "search ban")
        offered = Protocol.offerOf gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.TurnFaceUp oid TurnUpProcedure.Manifest, Action.Ignore oid ban, Action.Plot oid cost])
        acting kind extra = R.Object ([(Text.pack "action", R.Text (Text.pack kind)), (Text.pack "object", Protocol.object oid)] <> [(Text.pack key, value) | (key, value) <- extra])
    Spec.assertEqWith s "offer" offered (R.Array [acting "TurnFaceUp" [("procedure", R.Text (Text.pack "Manifest"))], acting "Ignore" [("abilityName", R.Text (Text.pack "search ban"))], acting "Plot" [("cost", Protocol.payment cost)]])
  Spec.it s "CR 305.1 / 708.6 a land play is a PlayLand, and a face-down cast is offered with its reason" $ do
    let oid = ObjectId.MkObjectId 7
        gs = Setup.emptyGame S.bothPlayers
        name = CardName.MkCardName (Text.pack "Ainok Tracker")
        offered = Protocol.offerOf gs (Prompt.ChooseAction (Decider.MkDecider S.alice) S.alice [Action.Play oid Nothing, Action.Cast oid name Facing.FaceUp, Action.Cast oid name (Facing.FaceDown (FaceDownState.defaultFor FaceDownReason.Morphed))])
        acting kind extra = R.Object ([(Text.pack "action", R.Text (Text.pack kind)), (Text.pack "object", Protocol.object oid)] <> [(Text.pack key, value) | (key, value) <- extra])
        casting = ("name", R.Text (Text.pack "Ainok Tracker"))
    Spec.assertEqWith s "offer" offered (R.Array [acting "PlayLand" [], acting "Cast" [casting], acting "Cast" [casting, ("faceDown", R.Text (Text.pack "Morphed"))]])

  Spec.it s "CR 601.2b / 702.152a / 101.4b a choice of cost offers each candidate's keyword, and it and an offer to pay what earlier payers answered" $ do
    let oid = ObjectId.MkObjectId 7
        taken = ObjectId.MkObjectId 9
        gs = Setup.emptyGame S.bothPlayers
        cost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) []
        blitz = Keyword.Blitz cost
        earlier = Seq.fromList [(S.bob, PayerAnswer.Paid cost (Set.singleton taken)), (S.alice, PayerAnswer.Declined)]
        choosing = Protocol.offerOf gs (Prompt.ChooseCost (Decider.MkDecider S.alice) S.alice oid [CandidateCost.plain Nothing cost, CandidateCost.plain (Just blitz) cost] earlier)
        paying = Protocol.offerOf gs (Prompt.ChooseToPay (Decider.MkDecider S.alice) S.alice oid PayOffer.InsteadOfDraw cost earlier)
        player p = R.Text (Text.pack (show (PlayerId.unwrap p)))
        told = R.Array [R.Array [player S.bob, R.Array [Protocol.payment cost, R.Array [Protocol.object taken]]], R.Array [player S.alice, R.Null]]
    Spec.assertEqWith s "the keywords" (fmap Codec.Reply.toValue (field "keywords" choosing)) (Just (Codec.encode (Common.list (Common.maybe Codec.Keyword.codec)) [Nothing, Just blitz]))
    Spec.assertEqWith s "the costs, equal" (field "costs" choosing) (Just (R.Array [Protocol.payment cost, Protocol.payment cost]))
    Spec.assertEqWith s "CR 101.4b earlier payers of a choice of cost" (field "earlier" choosing) (Just told)
    Spec.assertEqWith s "CR 101.4b earlier payers of an offer to pay" (field "earlier" paying) (Just told)
    -- CR 603.7g: the delayed ability a candidate creates, only where one does.
    let named = AbilityName.MkAbilityName (Text.pack "sacrifice it")
        arming = Protocol.offerOf gs (Prompt.ChooseCost (Decider.MkDecider S.alice) S.alice oid [CandidateCost.plain Nothing cost, (CandidateCost.plain Nothing cost) {CandidateCost.arms = Just named}] Seq.empty)
    Spec.assertEqWith s "CR 603.7g the delayed abilities" (field "arming" arming) (Just (R.Array [R.Null, R.Text (Text.pack "sacrifice it")]))
    Spec.assertEqWith s "and none where no candidate creates one" (field "arming" choosing) Nothing

  Spec.it s "CR 510.1 a combat damage assignment offers each recipient's lethal damage and what is already paid in the answer's vocabulary" $ do
    let o7 = ObjectId.MkObjectId 7
        lethal = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 9), 2), (Recipient.ToCreature (ObjectId.MkObjectId 11), 3 :: Natural)]
        paid = Map.singleton (Recipient.ToCreature (ObjectId.MkObjectId 11)) (1 :: Natural)
        prompt = Prompt.AssignCombatDamage (Decider.MkDecider S.alice) S.alice o7 lethal paid 5
        offer = Protocol.offerOf (Setup.emptyGame S.bothPlayers) prompt
    Spec.assertEqWith s "lethal" (fmap (Protocol.decodeAnswer prompt) (field "lethal" offer)) (Just (Right lethal))
    Spec.assertEqWith s "CR 702.19b: what other creatures already assign" (fmap (Protocol.decodeAnswer prompt) (field "paid" offer)) (Just (Right paid))
    Spec.assertEqWith s "total" (field "total" offer) (Just (R.Number 5))
    Spec.assertEqWith s "source" (field "source" offer) (Just (Protocol.object o7))

  Spec.it s "CR 400.2 a guess offers its source and proposition and never the card" $ do
    let o7 = ObjectId.MkObjectId 7
        proposition = Filter.Type.Measures (Measures.MkMeasures Measure.ManaValue Comparison.GreaterThan (Operand.Literal 4))
        offer = Protocol.offerOf (Setup.emptyGame S.bothPlayers) (Prompt.ChooseGuess (Decider.MkDecider S.bob) S.bob o7 proposition)
        keys = case offer of
          R.Object pairs -> fmap fst pairs
          _ -> []
    Spec.assertEqWith s "keys" keys [Text.pack "source", Text.pack "proposition"]
    Spec.assertEqWith s "source" (field "source" offer) (Just (Protocol.object o7))
    Spec.assertEqWith s "proposition" (fmap Codec.Reply.toValue (field "proposition" offer)) (Just (Codec.encode (Codec.Filter.codec Codec.Keyword.codec) proposition))

  Spec.it s "CR 118.12 an offer to pay names the cost and what offers it" $ do
    vane <- S.printingOf s registry "Arcum's Weathervane"
    let (oid, gs) = S.addPermanent vane S.alice (Setup.emptyGame S.bothPlayers)
        costs = fmap ActivatedAbility.cost (Projection.abilitiesOf oid gs)
        offers = [Protocol.offerOf gs (Prompt.ChooseToPay (Decider.MkDecider S.bob) S.bob oid (PayOffer.AtClause (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0)) cost mempty) | cost <- costs]
        decodedCost offer = fmap (\c -> Common.parse (Common.render (Codec.Reply.toValue c)) >>= Codec.decode (Codec.Cost.codec Codec.Keyword.codec)) (field "cost" offer)
    Spec.assertEqWith s "cost" (fmap decodedCost offers) (fmap (Just . Right) costs)
    Spec.assertEqWith s "source" (fmap (field "source") offers) (fmap (const (Just (Protocol.object oid))) costs)

  Spec.it s "CR 201.4 a prompt answered by a name or a word offers what bounds the answer" $ do
    let o7 = ObjectId.MkObjectId 7
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        options prompt = maybe (Left (Text.pack "no options")) Right (field "options" (Protocol.offerOf gs prompt) >>= arrayOf) >>= traverse (Protocol.decodeAnswer prompt)
        restriction = Filter.Type.Not (Filter.Type.HasCardType CardType.Land)
        naming = Protocol.offerOf gs (Prompt.ChooseCardName alice S.alice o7 restriction (Seq.singleton (S.bob, CardName.MkCardName (Text.pack "Hill Giant"))))
        basic = Prompt.ChooseSubtype alice S.alice o7 SubtypeFamily.BasicLandType Nothing
        creature = Prompt.ChooseSubtype alice S.alice o7 SubtypeFamily.CreatureType Nothing
        walker = Prompt.ChooseSubtype alice S.alice o7 SubtypeFamily.PlaneswalkerType Nothing
        listed = Prompt.ChooseSubtype alice S.alice o7 SubtypeFamily.CreatureType (Just (Subtype.Treefolk NonEmpty.:| [Subtype.Kithkin]))
        families = Set.fromList [WordFamily.BasicLandType, WordFamily.Color]
        forbidden = Set.singleton (TextWord.ColorWord Color.Black)
        swap = Protocol.offerOf gs (Prompt.ChooseWordSwap alice S.alice o7 (SlotName.MkSlotName (Text.pack "target")) families forbidden)
    Spec.assertEqWith s "CR 201.4a a name's restriction, beside its source" (field "source" naming, fmap Codec.Reply.toValue (field "filter" naming)) (Just (Protocol.object o7), Just (Codec.encode (Codec.Filter.codec Codec.Keyword.codec) restriction))
    Spec.assertEqWith s "CR 101.4b the names earlier choosers chose" (field "earlier" naming) (Just (R.Array [R.Array [R.Text (Text.pack "1"), R.Text (Text.pack "Hill Giant")]]))
    Spec.assertEqWith s "CR 305.6 the basic land types" (options basic) (Right [Subtype.Forest, Subtype.Island, Subtype.Mountain, Subtype.Plains, Subtype.Swamp])
    Spec.assertEqWith s "CR 205.3m every creature type" (fmap Set.fromList (options creature)) (Right Engine.Subtype.everyCreatureType)
    Spec.assertEqWith s "CR 205.3j every planeswalker type" (fmap Set.fromList (options walker)) (Right (Set.fromList (filter Engine.Subtype.isPlaneswalkerType [minBound ..])))
    Spec.assertBool s (either (const False) (all (Scenario.withinOffer creature)) (options creature)) "every creature type within the offer"
    Spec.assertEqWith s "CR 614.12a only the creature types a card lists" (options listed) (Right [Subtype.Kithkin, Subtype.Treefolk])
    Spec.assertBool s (not (Scenario.withinOffer listed Subtype.Goblin)) "a creature type off the list is outside the offer"
    Spec.assertEqWith s "CR 612.2 a swap's families and forbidden words, beside its source and slot" (fmap (\key -> field key swap) ["source", "slot", "families", "forbidden"]) [Just (Protocol.object o7), Just (R.Text (Text.pack "target")), Just (R.Array [R.Text (Text.pack "BasicLandType"), R.Text (Text.pack "Color")]), Just (Reply.encode (Reply.list Reply.textWord) Protocol.namer [TextWord.ColorWord Color.Black])]

  Spec.it s "CR 608.2h a departed option is named beside the options by last known information, where it left a public zone" $ do
    bears <- S.printingOf s registry "Grizzly Bears"
    piker <- S.printingOf s registry "Goblin Piker"
    let (live, g1) = S.addPermanent bears S.alice (Setup.emptyGame S.bothPlayers)
        (dead, g2) = S.addPermanent piker S.alice g1
        (held, g3) = S.addHandCard piker S.bob g2
        gs = Event.forgetObject (S.runPure S.identityAnswer g3 (Event.destroy (pure ()) Regenerability.Regenerable [dead])) held
        offer among = Protocol.offerOf gs (Prompt.ChooseCardFromAmong (Decider.MkDecider S.alice) S.alice live among)
        gone = offer (live NonEmpty.:| [dead, held])
    Spec.assertEqWith s "the Piker and the hand card have left the game" (fmap (`Map.member` GameState.objects gs) [live, dead, held]) [True, False, False]
    Spec.assertEqWith s "every candidate is an option" (field "options" gone) (Just (R.Array (fmap Protocol.object [live, dead, held])))
    Spec.assertEqWith s "CR 400.2 the permanent's name, and not the hidden hand card's" (field "departed" gone) (Just (R.Object [(Text.pack ("$" <> show (ObjectId.unwrap dead)), R.Text (Text.pack "Goblin Piker"))]))
    Spec.assertEqWith s "nothing where every option is in the game" (field "departed" (offer (pure live))) Nothing
  Spec.it s "CR 404.3 a graveyard batch offers its cards top first, and an arrangement reads back" $ do
    let o5 = ObjectId.MkObjectId 5
        o7 = ObjectId.MkObjectId 7
        prompt = Prompt.ArrangeGraveyardArrivals (Decider.MkDecider S.alice) S.alice [o7, o5]
        offer = Protocol.offerOf (Setup.emptyGame S.bothPlayers) prompt
    Spec.assertEqWith s "from" (field "from" offer) (Just (Protocol.objects [o7, o5]))
    Spec.assertEqWith s "in order" (Protocol.decodeAnswer prompt (R.Array [R.Number 1, R.Number 0])) (Right (GraveyardArrangement.InOrder [1, 0]))
    Spec.assertEqWith s "any order" (Protocol.decodeAnswer prompt R.Null) (Right GraveyardArrangement.AnyOrder)

  Spec.it s "a prompt answered by one of a list offers each option as the answer that picks it" $ do
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        o11 = ObjectId.MkObjectId 11
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        listed key prompt = do
          let offer = Protocol.offerOf gs prompt
          options <- maybe (Left (Text.pack ("no " <> key))) Right (field key offer >>= arrayOf)
          answers <- traverse (Protocol.decodeAnswer prompt) options
          pure (field "source" offer, answers)
        hand = Prompt.ChooseCardInHand alice S.alice o7 (o9 NonEmpty.:| [o11])
    Spec.assertEqWith s "cards" (listed "options" hand) (Right (Just (Protocol.object o7), [o9, o11]))
    Spec.assertEqWith s "players" (listed "players" (Prompt.ChoosePlayer alice S.alice o7 (S.alice NonEmpty.:| [S.bob]))) (Right (Just (Protocol.object o7), [S.alice, S.bob]))
    Spec.assertEqWith s "a may declines last" (listed "players" (Prompt.ChooseAssistant alice S.alice o7 (S.bob NonEmpty.:| []))) (Right (Just (Protocol.object o7), [Just S.bob, Nothing]))
    Spec.assertEqWith s "words" (listed "options" (Prompt.ChooseColor alice S.alice o7 (Color.Red NonEmpty.:| [Color.Green]))) (Right (Just (Protocol.object o7), [Color.Red, Color.Green]))
    Spec.assertEqWith s "attack targets" (listed "options" (Prompt.ChooseAttackTarget alice S.alice o7 (AttackTarget.OfPlayer S.bob NonEmpty.:| [AttackTarget.OfPlaneswalker o9]))) (Right (Just (Protocol.object o7), [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlaneswalker o9]))
    Spec.assertEqWith s "no source" (listed "players" (Prompt.ChooseActivePlayer alice S.alice (S.bob NonEmpty.:| [S.alice]))) (Right (Nothing, [S.bob, S.alice]))
    Spec.assertEqWith s "turn markers" (listed "options" (Prompt.ChooseTurnMarker alice S.alice (2 NonEmpty.:| [1]))) (Right (Nothing, [2, 1]))
    Spec.assertBool s (all (Scenario.withinOffer hand) [o9, o11]) "every option within the offer"
    -- CR 101.4b: what earlier choosers took.
    let graveyard = Protocol.offerOf gs (Prompt.ChooseCardInGraveyard alice S.alice o7 (o9 NonEmpty.:| []) (Seq.singleton (S.bob, o11)))
    Spec.assertEqWith s "earlier" (field "earlier" graveyard) (Just (R.Array [R.Array [R.Text (Text.pack "1"), Protocol.object o11]]))

  Spec.it s "CR 401.4 a prompt answered by a permutation offers each item it orders, and every permutation is within the offer" $ do
    giant <- S.printingOf s registry "Hill Giant"
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        o11 = ObjectId.MkObjectId 11
        alice = Decider.MkDecider S.alice
        (printing, gs) = Game.intern giant (Setup.emptyGame S.bothPlayers)
        offerOf = Protocol.offerOf gs
        both = Just (R.Array [Protocol.object o9, Protocol.object o11])
        components = [CostComponent.TapThis, CostComponent.SacrificeThis]
        hit = DamageEvent.MkDamageEvent o7 (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing Nothing mempty False mempty DamageKind.Noncombat
        arrivals = Prompt.ArrangeLibraryArrivals alice S.alice LibraryPosition.Bottom [o9, o11]
        cost = Prompt.OrderCostComponents alice S.alice o7 components
        cards = Prompt.OrderComponentCards alice S.alice Zone.Graveyard [printing, printing]
        each = Prompt.OrderForEach alice S.alice o7 [Recipient.ToCreature o9, Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "library cards" (field "order" (offerOf (Prompt.ArrangeLibraryCards alice S.alice [o9, o11]))) both
    Spec.assertEqWith s "library arrivals, beside their end" (fmap (\key -> field key (offerOf arrivals)) ["end", "order"]) [Just (R.Text (Text.pack "Bottom")), both]
    Spec.assertEqWith s "timestamps" (field "order" (offerOf (Prompt.OrderTimestamps alice S.alice [o9, o11]))) both
    Spec.assertEqWith s "mana activations" (field "order" (offerOf (Prompt.OrderManaActivations alice S.alice [o9, o11]))) both
    Spec.assertEqWith s "combat tolls" (field "order" (offerOf (Prompt.OrderCombatTolls alice S.alice [o9, o11]))) both
    Spec.assertEqWith s "component cards by name, beside their zone" (fmap (\key -> field key (offerOf cards)) ["zone", "order"]) [Just (R.Text (Text.pack "Graveyard")), Just (R.Array (replicate 2 (R.Text (Text.pack "Hill Giant"))))]
    Spec.assertEqWith s "cost components, beside their source" (field "source" (offerOf cost), fmap (traverse (either (const Nothing) Just . Codec.decode (Codec.CostComponent.codec Codec.Keyword.codec) . Codec.Reply.toValue)) (field "order" (offerOf cost) >>= arrayOf)) (Just (Protocol.object o7), Just (Just components))
    Spec.assertEqWith s "damage events" (field "order" (offerOf (Prompt.OrderDamage alice S.alice [hit]))) (Just (R.Array [R.Object [(Text.pack "source", Protocol.object o7), (Text.pack "target", R.Array [R.Text (Text.pack "Player"), R.Text (Text.pack "1")]), (Text.pack "amount", R.Number 2)]]))
    Spec.assertEqWith s "recipients, beside their source" (field "source" (offerOf each), field "order" (offerOf each)) (Just (Protocol.object o7), Just (Reply.encode (Reply.list Reply.recipient) Protocol.namer [Recipient.ToCreature o9, Recipient.ToPlayer S.bob]))
    Spec.assertBool s (all (Scenario.withinOffer arrivals) [[0, 1], [1, 0]] && not (Scenario.withinOffer arrivals [0, 0])) "each permutation within the offer, and only those"

  Spec.it s "a prompt answered by one of a fixed set offers each value as the answer that picks it, beside what it decides about" $ do
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        options prompt = maybe (Left (Text.pack "no options")) Right (field "options" (Protocol.offerOf gs prompt) >>= arrayOf) >>= traverse (Protocol.decodeAnswer prompt)
        dredge = Prompt.ChooseDredge alice S.alice o7 3
    Spec.assertEqWith s "a yes or no, yes first" (options dredge) (Right [OptionalDecision.Exercises, OptionalDecision.Declines])
    Spec.assertEqWith s "beside its source and count" (fmap (\key -> field key (Protocol.offerOf gs dredge)) ["source", "count"]) [Just (Protocol.object o7), Just (R.Number 3)]
    Spec.assertEqWith s "a coin's faces" (options (Prompt.CallCoin alice S.alice)) (Right [CoinFace.Heads, CoinFace.Tails])
    Spec.assertEqWith s "a land, a nonland, or neither" (options (Prompt.ChooseLandOrNonland alice S.alice o7)) (Right [Just LandOrNonland.Land, Just LandOrNonland.Nonland, Nothing])
    Spec.assertEqWith s "a mutate's sides" (options (Prompt.ChooseMutateSide alice S.alice o7 o9)) (Right [MutateSide.Over, MutateSide.Under])
    Spec.assertEqWith s "a library's ends" (options (Prompt.ChooseLibraryEnd alice S.alice o7 0)) (Right [LibraryPosition.Top, LibraryPosition.Bottom])
    Spec.assertEqWith s "a die reroll, with no source" (fmap (\key -> field key (Protocol.offerOf gs (Prompt.Rethrow alice S.alice (ThrowResult.DieCameUp 4) Nothing))) ["source", "result", "cost"]) [Nothing, Just (R.Number 4), Nothing]
    Spec.assertBool s (all (Scenario.withinOffer dredge) [OptionalDecision.Exercises, OptionalDecision.Declines]) "every option within the offer"

  Spec.it s "a prompt answered by a set offers each candidate as an element of the answer, within its bounds" $ do
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        o11 = ObjectId.MkObjectId 11
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        bounds prompt = fmap (\key -> field key (Protocol.offerOf gs prompt)) ["source", "least", "most"]
        sacrifices = Prompt.ChooseSacrifices alice S.alice o7 [o9, o11] 1 (Seq.singleton (S.bob, Set.singleton o7))
        loop = Prompt.ChooseLoopMembers alice S.alice o7 [Recipient.ToCreature o9, Recipient.ToPlayer S.bob]
    Spec.assertEqWith s "candidates read back as the answer" (fmap (Protocol.decodeAnswer sacrifices . R.Array) (field "among" (Protocol.offerOf gs sacrifices) >>= arrayOf)) (Just (Right (Set.fromList [o9, o11])))
    Spec.assertEqWith s "exactly the count" (bounds sacrifices) [Just (Protocol.object o7), Just (R.Number 1), Just (R.Number 1)]
    Spec.assertEqWith s "CR 101.4b what earlier choosers picked" (field "earlier" (Protocol.offerOf gs sacrifices)) (Just (R.Array [R.Array [R.Text (Text.pack "1"), R.Array [Protocol.object o7]]]))
    Spec.assertEqWith s "no more than there are" (bounds (Prompt.ChooseTaps alice S.alice o7 [o9] 2)) [Just (Protocol.object o7), Just (R.Number 1), Just (R.Number 1)]
    Spec.assertEqWith s "CR 702.167a one or more is a least" (bounds (Prompt.ChooseMaterials alice S.alice o7 [o9, o11] 1 True)) [Just (Protocol.object o7), Just (R.Number 1), Nothing]
    Spec.assertEqWith s "CR 702.122a a power threshold, unbounded" (fmap (\key -> field key (Protocol.offerOf gs (Prompt.ChooseTapsForTotalPower alice S.alice o7 [(o9, 2)] 3))) ["power", "least", "most"]) [Just (R.Number 3), Nothing, Nothing]
    Spec.assertEqWith s "any number up to the most" (bounds (Prompt.ChooseAnyNumber alice S.alice o7 AnyNumberOf.ToExile [o9, o11] (Just 1) Seq.empty)) [Just (Protocol.object o7), Just (R.Number 0), Just (R.Number 1)]
    Spec.assertEqWith s "recipients read back as the answer" (fmap (Protocol.decodeAnswer loop . R.Array) (field "among" (Protocol.offerOf gs loop) >>= arrayOf)) (Just (Right (Set.fromList [Recipient.ToCreature o9, Recipient.ToPlayer S.bob])))

  Spec.it s "a prompt answered by an index or an entry of what it lists offers each option as its answer, beside the entry it picks" $ do
    giant <- S.printingOf s registry "Hill Giant"
    intelligence <- S.printingOf s registry "Great Intelligence's Plan"
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        alice = Decider.MkDecider S.alice
        bobDecides = Decider.MkDecider S.bob
        (looked, g1) = S.addPermanent giant S.bob (Setup.emptyGame S.bothPlayers)
        (printing, g2) = Game.intern giant g1
        (plan, g3) = S.spellOnStack intelligence S.alice g2
        gs = g3 {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 0))}) plan (GameState.objects g3)}
        offered :: Prompt.Prompt r -> Either Text.Text ([r], Maybe R.Reply)
        offered prompt = do
          let offer = Protocol.offerOf gs prompt
          options <- maybe (Left (Text.pack "no options")) Right (field "options" offer >>= arrayOf)
          answers <- traverse (Protocol.decodeAnswer prompt) options
          if all (Scenario.withinOffer prompt) answers then Right (answers, field "entries" offer) else Left (Text.pack "an option outside the offer")
        entries = Just . R.Array
        tapped = ReplacementEntry.MkReplacementEntry looked (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.EntersTapState TapState.Tapped)))
        hit = DamageEvent.MkDamageEvent o7 (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing Nothing mempty False mempty DamageKind.Noncombat
        hidden = GameEvent.Looked (Looked.MkLooked looked (Projection.project looked gs) (Set.singleton S.bob))
        hillGiant = CardName.MkCardName (Text.pack "Hill Giant")
        clause = ClauseIndex.MkClauseIndex
        hand = HandActionIndex.MkHandActionIndex
        shape = EntryOption.MkEntryOption 1 6 Set.empty Set.empty
    Spec.assertEqWith s "CR 616.1 a replacement by its source and Oracle text" (offered (Prompt.ChooseReplacement alice S.alice [tapped, tapped])) (Right ([0, 1], entries (replicate 2 (R.Object [(Text.pack "source", Protocol.object looked), (Text.pack "text", R.Text (Text.pack "Hill Giant enters tapped."))]))))
    Spec.assertEqWith s "CR 614.1c an entry option as its shape" (fmap (fmap (fmap (fmap (Codec.decode Codec.EntryOption.codec . Codec.Reply.toValue)) . (>>= arrayOf))) (offered (Prompt.ChooseEntryOption alice S.alice o7 [shape]))) (Right ([0], Just [Right shape]))
    Spec.assertEqWith s "CR 603.7b an event the seat saw, and null for one it did not" (offered (Prompt.ChooseDelayedTriggerEvent alice S.alice o7 (GameEvent.DamageDealt hit NonEmpty.:| [hidden]))) (Right ([0, 1], entries [Reply.encode (Reply.viaCodec Codec.GameEvent.codec) Protocol.namer (GameEvent.DamageDealt hit), R.Null]))
    Spec.assertEqWith s "CR 706.2b a modifier by its object, null for none" (offered (Prompt.ChooseRollModifier alice S.alice (Just o7 NonEmpty.:| [Nothing]))) (Right ([0, 1], entries [R.Object [(Text.pack "source", Protocol.object o7)], R.Object [(Text.pack "source", R.Null)]]))
    Spec.assertEqWith s "CR 123.5c a keeper by its card's name" (offered (Prompt.ChooseStickerKeeper alice S.alice (printing NonEmpty.:| [printing]))) (Right ([0, 1], entries (replicate 2 (R.Text (Text.pack "Hill Giant")))))
    Spec.assertEqWith s "CR 315.5a a hand as its cards" (offered (Prompt.KeepHand alice S.alice ([o7] NonEmpty.:| [[]]))) (Right ([0, 1], entries [R.Array [Protocol.object o7], R.Array []]))
    Spec.assertEqWith s "CR 706.4 a result as its number" (offered (Prompt.ChooseDieResult alice S.alice (Just o7) (4 NonEmpty.:| [4]))) (Right ([0, 1], entries [R.Number 4, R.Number 4]))
    Spec.assertEqWith s "CR 701.48a a branch, then neither" (offered (Prompt.ChooseLearn alice S.alice o7 (LearnMode.TakeLesson NonEmpty.:| []))) (Right ([Just LearnMode.TakeLesson, Nothing], entries [R.Text (Text.pack "TakeLesson")]))
    Spec.assertEqWith s "CR 608.2d a clause, as its index where no object resolves, then neither where it is offered" (offered (Prompt.ChooseClause alice S.alice o7 (ModeIndex.MkModeIndex 0) (clause 1 NonEmpty.:| [clause 2]) True Seq.empty)) (Right ([Just (clause 1), Just (clause 2), Nothing], entries [R.Number 1, R.Number 2]))
    Spec.assertEqWith s "CR 701.55a a branch of a villainous choice as its Oracle text, and as its index where its clause does not render" (offered (Prompt.ChooseClause bobDecides S.bob plan (ModeIndex.MkModeIndex 0) (clause 1 NonEmpty.:| [clause 2]) False Seq.empty)) (Right ([Just (clause 1), Just (clause 2)], entries [R.Text (Text.pack "Target opponent discards three cards."), R.Number 2]))
    Spec.assertEqWith s "CR 101.4b and the branch an earlier chooser announced likewise" (field "earlier" (Protocol.offerOf gs (Prompt.ChooseClause bobDecides S.bob plan (ModeIndex.MkModeIndex 0) (clause 1 NonEmpty.:| [clause 2]) False (Seq.singleton (S.alice, Just (clause 1)))))) (Just (R.Array [R.Array [R.Text (Text.pack "0"), R.Text (Text.pack "Target opponent discards three cards.")]]))
    Spec.assertEqWith s "CR 701.55a and not where it is not" (fmap fst (offered (Prompt.ChooseClause alice S.alice o7 (ModeIndex.MkModeIndex 0) (clause 1 NonEmpty.:| [clause 2]) False Seq.empty))) (Right [Just (clause 1), Just (clause 2)])
    Spec.assertEqWith s "CR 101.4b the clauses earlier choosers announced" (field "earlier" (Protocol.offerOf gs (Prompt.ChooseClause alice S.alice o7 (ModeIndex.MkModeIndex 0) (clause 1 NonEmpty.:| [clause 2]) False (Seq.fromList [(S.bob, Just (clause 2)), (S.bob, Nothing)])))) (Just (R.Array [R.Array [R.Text (Text.pack "1"), R.Number 2], R.Array [R.Text (Text.pack "1"), R.Null]]))
    Spec.assertEqWith s "CR 700.3d a pile, an empty one included" (offered (Prompt.ChoosePile alice S.alice o7 (Set.singleton o9 NonEmpty.:| [Set.empty]) Seq.empty)) (Right ([Set.singleton o9, Set.empty], Nothing))
    Spec.assertEqWith s "CR 101.4b the piles earlier choosers took" (field "earlier" (Protocol.offerOf gs (Prompt.ChoosePile alice S.alice o7 (Set.singleton o9 NonEmpty.:| [Set.empty]) (Seq.singleton (S.bob, Set.singleton o9))))) (Just (R.Array [R.Array [R.Text (Text.pack "1"), R.Array [Protocol.object o9]]]))
    Spec.assertEqWith s "CR 103.5b a card's action, by its place where it has two, then none" (offered (Prompt.MulliganAction alice S.alice [(o7, hand 0), (o7, hand 1), (o9, hand 0)])) (Right ([Just (o7, hand 0), Just (o7, hand 1), Just (o9, hand 0), Nothing], entries [R.Object [(Text.pack "source", Protocol.object o7), (Text.pack "ability", R.Number 0)], R.Object [(Text.pack "source", Protocol.object o7), (Text.pack "ability", R.Number 1)], R.Object [(Text.pack "source", Protocol.object o9)]]))
    Spec.assertEqWith s "CR 103.6 an opening-hand action, then none" (fmap fst (offered (Prompt.OpeningHandAction alice S.alice [(o9, hand 0)]))) (Right [Just (o9, hand 0), Nothing])
    Spec.assertEqWith s "CR 608.2g a cast as its action, then none" (offered (Prompt.CastWhileSearching alice S.alice [(o9, hillGiant, Facing.FaceUp)])) (Right ([Just (o9, hillGiant, Facing.FaceUp), Nothing], entries [Offer.offer Protocol.namer gs (Action.Cast o9 hillGiant Facing.FaceUp)]))

  Spec.it s "a prompt answered by a number offers its least and greatest, and a die adjustment each die moved either way" $ do
    let o7 = ObjectId.MkObjectId 7
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        bounds :: Prompt.Prompt r -> [Maybe R.Reply]
        bounds prompt = fmap (\key -> field key (Protocol.offerOf gs prompt)) ["source", "least", "greatest"]
        kicker = Keyword.Kicker (Cost.Type.MkCost Nothing [])
        adjust = Prompt.AdjustDieRoll alice S.alice (4 NonEmpty.:| [-1]) 2 Nothing
        options = maybe (Left (Text.pack "no options")) Right (field "options" (Protocol.offerOf gs adjust) >>= arrayOf) >>= traverse (Protocol.decodeAnswer adjust)
    Spec.assertEqWith s "CR 601.2b a kicker from none to its limit" (bounds (Prompt.ChooseKicker alice S.alice o7 kicker Nothing (Just 1))) [Just (Protocol.object o7), Just (R.Number 0), Just (R.Number 1)]
    Spec.assertEqWith s "CR 702.33c and with no greatest for any number of times" (bounds (Prompt.ChooseKicker alice S.alice o7 kicker Nothing Nothing)) [Just (Protocol.object o7), Just (R.Number 0), Nothing]
    Spec.assertEqWith s "beside the keyword paid" (field "keyword" (Protocol.offerOf gs (Prompt.ChooseKicker alice S.alice o7 kicker Nothing Nothing))) (Just (Reply.encode (Reply.viaCodec Codec.Keyword.codec) Protocol.namer kicker))
    Spec.assertEqWith s "and a count reads back as the decision" (Protocol.decodeAnswer (Prompt.ChooseKicker alice S.alice o7 kicker Nothing Nothing) (R.Number 3)) (Right (KickerDecision.MkKickerDecision 3))
    Spec.assertEqWith s "CR 702.132a an assist from none to the pool's most" (bounds (Prompt.ChooseAssistAmount alice S.alice o7 3)) [Just (Protocol.object o7), Just (R.Number 0), Just (R.Number 3)]
    Spec.assertEqWith s "CR 107.14 energy from none to the player's" (bounds (Prompt.ChoosePaidEnergy alice S.alice o7 5)) [Just (Protocol.object o7), Just (R.Number 0), Just (R.Number 5)]
    Spec.assertEqWith s "CR 715.3b a chapter from one to the final" (bounds (Prompt.ChooseReadAheadChapter alice S.alice o7 3)) [Just (Protocol.object o7), Just (R.Number 1), Just (R.Number 3)]
    Spec.assertEqWith s "CR 706.2b each die either way, then none" options (Right ([Just (index, adjustment) | index <- [0, 1], adjustment <- [RollAdjustment.Increase, RollAdjustment.Decrease]] <> [Nothing]))
    Spec.assertEqWith s "beside the results, unclamped, and the amount" (fmap (\key -> field key (Protocol.offerOf gs adjust)) ["results", "amount", "cost"]) [Just (R.Array [R.Number 4, R.Number (-1)]), Just (R.Number 2), Nothing]
    Spec.assertBool s (all (Scenario.withinOffer (Prompt.ChooseAssistAmount alice S.alice o7 3)) [0, 3] && not (Scenario.withinOffer (Prompt.ChooseAssistAmount alice S.alice o7 3) 4)) "the bounds are the offer's"

  Spec.it s "CR 700.2a a modal spell's legal modes are each named by its Oracle text" $ do
    charm <- S.printingOf s registry "Ebony Charm"
    let (spell, gs) = S.addHandCard charm S.alice (Setup.emptyGame S.bothPlayers)
        modes = Prompt.ChooseModes (Decider.MkDecider S.alice) S.alice spell (Set.fromList (fmap ModeIndex.MkModeIndex [0, 2])) (ModeSelection.ChooseExactly 1)
    Spec.assertEqWith s "the first and third modes" (field "names" (Protocol.offerOf gs modes)) (Just (R.Array (fmap (R.Text . Text.pack) ["Target opponent loses 1 life and you gain 1 life.", "Target creature gains fear until end of turn."])))

  Spec.it s "a prompt answered by a set or a sequence offers each candidate as an element of the answer" $ do
    giant <- S.printingOf s registry "Hill Giant"
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        alice = Decider.MkDecider S.alice
        (card, g1) = S.addPermanent giant S.bob (Setup.emptyGame S.bothPlayers)
        (other, g2) = S.addPermanent giant S.bob g1
        (printing, gs) = Game.intern giant g2
        free = Cost.Type.MkCost Nothing []
        among :: Prompt.Prompt r -> [R.Reply]
        among prompt = Maybe.fromMaybe [] (field "among" (Protocol.offerOf gs prompt) >>= arrayOf)
        told :: Prompt.Prompt r -> [String] -> [Maybe R.Reply]
        told prompt = fmap (\key -> field key (Protocol.offerOf gs prompt))
        mode = ModeIndex.MkModeIndex
        modes = Prompt.ChooseModes alice S.alice o7 (Set.fromList [mode 0, mode 2]) (ModeSelection.ChooseExactlyWithRepeats 2)
        spliceCost = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)])) []
        splice = Prompt.ChooseSplice alice S.alice o7 [(o9, free), (card, spliceCost)]
        zones = Prompt.ChooseSearchZones alice S.alice (Set.fromList [SearchPlace.InZone Zone.Graveyard, SearchPlace.OutsideTheGame])
        pool = OutsideCard.InPool printing
        outside = Prompt.ChooseFromOutsideTheGame alice S.alice ((pool, 2) NonEmpty.:| []) 1 2
        bands = Prompt.DeclareBands alice S.alice [o9, card]
        scry = Prompt.ChooseScry alice S.alice [card, other]
    Spec.assertEqWith s "CR 700.2a the legal modes by index" (among modes) [R.Number 0, R.Number 2]
    Spec.assertEqWith s "CR 700.2d as many as the selection takes, one chosen again" (told modes ["least", "most", "repeats"]) [Just (R.Number 2), Just (R.Number 2), Just (R.Boolean True)]
    Spec.assertEqWith s "and null for a mode with no text" (told modes ["names"]) [Just (R.Array [R.Null, R.Null])]
    Spec.assertEqWith s "CR 700.2i each listed mode's pawprints" (told (Prompt.ChooseModes alice S.alice o7 (Set.fromList [mode 0, mode 2]) (ModeSelection.ChoosePawprintsWithRepeats (ChoosePawprints.MkChoosePawprints 5 (Seq.fromList [1, 2, 3])))) ["most", "pawprints"]) [Just (R.Number 5), Just (R.Array [R.Number 1, R.Number 3])]
    Spec.assertEqWith s "and none for a selection by count" (told modes ["pawprints"]) [Nothing]
    Spec.assertEqWith s "and a mode named twice reads back" (Protocol.decodeAnswer modes (R.Array (replicate 2 (R.Number 2)))) (Right (Seq.fromList [mode 2, mode 2]))
    Spec.assertEqWith s "CR 702.47a each card beside the splice cost it adds" (told splice ["costs"]) [Just (R.Array (fmap Protocol.payment [free, spliceCost]))]
    Spec.assertEqWith s "CR 702.47b the cards read back in the order named" (Protocol.decodeAnswer splice (R.Array (reverse (among splice)))) (Right [card, o9])
    Spec.assertEqWith s "CR 701.23j at least one place, each named" (told zones ["least", "names"]) [Just (R.Number 1), Just (R.Array (fmap R.Text [Text.pack "a graveyard", Text.pack "outside the game"]))]
    Spec.assertEqWith s "and the places read back" (Protocol.decodeAnswer zones (R.Array (among zones))) (Right (Set.fromList [SearchPlace.InZone Zone.Graveyard, SearchPlace.OutsideTheGame]))
    Spec.assertEqWith s "CR 701.34a the permanents, then the players as recipients" (among (Prompt.ChooseProliferate alice S.alice [o9] [S.bob])) [Protocol.object o9, Reply.encode Reply.recipient Protocol.namer (Recipient.ToPlayer S.bob)]
    Spec.assertEqWith s "CR 400.11c a card once per copy, named" (told outside ["names", "least", "most"]) [Just (R.Array (replicate 2 (R.Text (Text.pack "Hill Giant (sideboard)")))), Just (R.Number 1), Just (R.Number 2)]
    Spec.assertEqWith s "and both copies read back" (Protocol.decodeAnswer outside (R.Array (among outside))) (Right [pool, pool])
    Spec.assertEqWith s "CR 702.22c the attackers, a band of them reading back" (Protocol.decodeAnswer bands (R.Array [R.Array (among bands)])) (Right [Set.fromList [o9, card]])
    Spec.assertEqWith s "CR 701.56a the candidates and the source" (told (Prompt.ChooseTimeTravel alice S.alice o7 [o9]) ["source", "among"]) [Just (Protocol.object o7), Just (R.Array [Protocol.object o9])]
    Spec.assertEqWith s "CR 701.22a the cards looked at, top first, each named" (told scry ["from", "names"]) [Just (R.Array [Protocol.object card, Protocol.object other]), Just (R.Array (replicate 2 (R.Text (Text.pack "Hill Giant"))))]
    Spec.assertEqWith s "CR 701.25a a surveil alike" (Protocol.offerOf gs (Prompt.ChooseSurveil alice S.alice [card, other])) (Protocol.offerOf gs scry)
    Spec.assertEqWith s "CR 701.29a a fateseal names whose library" (told (Prompt.ChooseFateseal alice S.alice S.bob [card]) ["owner", "from"]) [Just (R.Text (Text.pack "1")), Just (R.Array [Protocol.object card])]

  Spec.it s "a prompt answered by an amount per item offers the answer each item taking the most it may, within the bounds on their sum" $ do
    let o7 = ObjectId.MkObjectId 7
        o9 = ObjectId.MkObjectId 9
        o11 = ObjectId.MkObjectId 11
        alice = Decider.MkDecider S.alice
        gs = Setup.emptyGame S.bothPlayers
        told :: Prompt.Prompt r -> [String] -> [Maybe R.Reply]
        told prompt = fmap (\key -> field key (Protocol.offerOf gs prompt))
        caps :: Prompt.Prompt r -> Maybe (Either Text.Text r)
        caps prompt = fmap (Protocol.decodeAnswer prompt) (field "amounts" (Protocol.offerOf gs prompt))
        plus = CounterKind.PlusOnePlusOne
        carried = Map.fromList [(o9, 2), (o11, 1 :: Natural)]
        kinds = Map.fromList [(plus, 2), (CounterKind.Time, 1)] :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural
        hit amount = DamageEvent.MkDamageEvent o7 (Recipient.ToPlayer S.bob) amount False False False 0 Nothing Nothing mempty False mempty DamageKind.Noncombat
        allocate = Prompt.AllocateDamage alice S.alice [hit 2, hit 3] 4
        slot = SlotName.MkSlotName (Text.pack "target")
        recipients = Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToCreature o9]
        announce = Prompt.AnnounceTargets alice S.alice o7 (Map.singleton slot (TargetCount.MkTargetCount 1 Nothing, recipients))
        mixed spread = Prompt.ChooseMixedCounterRemoval alice S.alice o7 spread 2 (Map.singleton o9 kinds)
        moved = Prompt.ChooseMovedCounters alice S.alice o7 o9 kinds
        distributed spread = Prompt.ChooseDistributedMovedCounters alice S.alice o7 spread kinds (o9 NonEmpty.:| [o11])
        distribution = Prompt.ChooseDistribution alice S.alice o7 3 (o9 NonEmpty.:| [o11])
        stored = Map.singleton (StoredResult.MkStoredResult 20 7) (2 :: Natural)
        redistribution = Prompt.ChooseRedistribution alice S.alice [(S.alice, 20), (S.bob, -3)]
        player = Reply.encode Reply.player Protocol.namer
    Spec.assertEqWith s "CR 615.7 each event's own amount" (caps allocate) (Just (Right [2, 3]))
    Spec.assertEqWith s "beside each event, the points divided whole" (told allocate ["entries", "least", "most"]) [Just (R.Array (fmap (Protocol.damageEvent . hit) [2, 3])), Just (R.Number 4), Just (R.Number 4)]
    Spec.assertEqWith s "CR 601.2c any number of targets up to what the board supplies" (caps announce) (Just (Right (Map.singleton slot 2)))
    Spec.assertEqWith s "from its least" (fmap (Protocol.decodeAnswer announce) (field "floors" (Protocol.offerOf gs announce))) (Just (Right (Map.singleton slot 1)))
    Spec.assertEqWith s "beside its source and recipients" (told announce ["source", "recipients"]) [Just (Protocol.object o7), Just (Reply.encode (Reply.mapOf (Reply.viaCodec Codec.SlotName.codec) (Reply.setOf Reply.recipient)) Protocol.namer (Map.singleton slot recipients))]
    Spec.assertEqWith s "no floor for up to" (told (Prompt.AnnounceTargets alice S.alice o7 (Map.singleton slot (TargetCount.upTo 2, recipients))) ["floors"]) [Nothing]
    Spec.assertEqWith s "CR 601.2h what each permanent carries" (caps (Prompt.ChooseCounterRemovalAmong alice S.alice o7 2 carried)) (Just (Right carried))
    Spec.assertEqWith s "exactly the count" (told (Prompt.ChooseCounterRemovalAmong alice S.alice o7 2 carried) ["source", "least", "most"]) [Just (Protocol.object o7), Just (R.Number 2), Just (R.Number 2)]
    Spec.assertEqWith s "at least it" (told (Prompt.ChooseCounterRemovalAtLeast alice S.alice o7 1 carried) ["least", "most"]) [Just (R.Number 1), Nothing]
    Spec.assertEqWith s "CR 608.2d at most it" (told (Prompt.ChooseCounterRemovalUpTo alice S.alice o7 2 carried) ["least", "most"]) [Nothing, Just (R.Number 2)]
    Spec.assertEqWith s "CR 122.1 each kind each permanent carries" (caps (mixed CounterSpread.FromAmong)) (Just (Right (Map.singleton o9 kinds)))
    Spec.assertEqWith s "off one permanent where the cost says one" (told (mixed CounterSpread.FromOne) ["fromOne", "least", "most"]) [Just (R.Boolean True), Just (R.Number 2), Just (R.Number 2)]
    Spec.assertEqWith s "and at least the count where it says one or more" (told (mixed CounterSpread.FromAmongAtLeast) ["fromOne", "least", "most"]) [Nothing, Just (R.Number 2), Nothing]
    Spec.assertEqWith s "CR 122.5 each kind's tally" (caps moved) (Just (Right kinds))
    Spec.assertEqWith s "off one object and onto another, any number" (told moved ["off", "onto", "least", "most"]) [Just (Protocol.object o7), Just (Protocol.object o9), Nothing, Nothing]
    Spec.assertEqWith s "one or more" (told (Prompt.ChooseMovedCountersAtLeastOne alice S.alice o7 o9 kinds) ["least"]) [Just (R.Number 1)]
    Spec.assertEqWith s "each destination may take each whole tally" (caps (distributed MoveSpread.AnyNumber)) (Just (Right (Map.fromList [(o9, kinds), (o11, kinds)])))
    Spec.assertEqWith s "beside the tallies, every counter" (told (distributed MoveSpread.Exactly) ["off", "counters", "least", "most"]) [Just (Protocol.object o7), Just (Reply.encode (Reply.mapOf Reply.counterKind Reply.natural) Protocol.namer kinds), Just (R.Number 3), Just (R.Number 3)]
    Spec.assertEqWith s "CR 609.3 that many of one kind" (told (distributed (MoveSpread.OneKind 2)) ["oneKind", "least", "most"]) [Just (R.Number 2), Nothing, Just (R.Number 2)]
    Spec.assertEqWith s "one or more, and up to one" (told (distributed MoveSpread.AtLeastOne) ["least"] <> told (distributed MoveSpread.UpToOne) ["most"]) [Just (R.Number 1), Just (R.Number 1)]
    Spec.assertEqWith s "CR 608.2d the whole count on any candidate" (caps distribution) (Just (Right (Map.fromList [(o9, 3), (o11, 3)])))
    Spec.assertEqWith s "divided exactly" (told distribution ["source", "least", "most"]) [Just (Protocol.object o7), Just (R.Number 3), Just (R.Number 3)]
    Spec.assertEqWith s "CR 706.8b each stored result by how many" (caps (Prompt.ChooseStoredRerolls alice S.alice o7 stored)) (Just (Right stored))
    Spec.assertEqWith s "CR 119.7 each candidate beside its life total" (told redistribution ["totals"]) [Just (R.Array [R.Array [player S.alice, R.Number 20], R.Array [player S.bob, R.Number (-3)]])]
    Spec.assertEqWith s "and a swap reads back" (Protocol.decodeAnswer redistribution (R.Array [R.Array [player S.alice, player S.bob], R.Array [player S.bob, player S.alice]])) (Right (Map.fromList [(S.alice, S.bob), (S.bob, S.alice)]))

  Spec.it s "a hidden card reaches no line sent to the other seat" $ do
    gs <- started s registry
    let logged = State.execState (Driver.drive [observer] (Protocol.over trusting (exchanging client) chance) 10000 (pure . (>= 5) . (TurnRegisters.turnNumber . GameState.turn)) gs) []
        sent = linesWithState logged
        to pid = [(at, line) | (at, line) <- sent, fmap (Question.seat . Request.question) (requestOf line) == Just pid]
        leaks (at, line) =
          let hidden = hiddenOf S.alice at
              names = Set.unions (fmap (`Projection.namesOf` at) hidden) `Set.difference` seenBy S.bob at
           in any (\name -> Text.unpack (CardName.unwrap name) `List.isInfixOf` line) (Set.toList names)
                || any (\oid -> ("\"$" <> show (ObjectId.unwrap oid) <> "\"") `List.isInfixOf` line) hidden
    Spec.assertBool s (not (null (to S.alice))) "alice asked"
    Spec.assertBool s (not (null (to S.bob))) "bob asked"
    Spec.assertBool s (not (any leaks (to S.bob))) "no leak"
  Spec.it s "a prompt no seat answers is never sent" $ do
    Spec.assertBool s (Maybe.isNothing (Protocol.questionOf S.threePlayerGame (Prompt.Shuffle [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]))) "shuffle"
    Spec.assertBool s (Maybe.isNothing (Protocol.questionOf S.threePlayerGame (Prompt.RollDie 6))) "die"
  Spec.it s "a reply that cannot be read is asked again with the problem" $ do
    gs <- started s registry
    let garbled = State.execState (Driver.drive [observer] (Protocol.over trusting (exchanging once) chance) 10000 (pure . (>= 3) . (TurnRegisters.turnNumber . GameState.turn)) gs) []
        problems = fmap (fmap Request.problem . requestOf . snd) (linesWithState garbled)
    case problems of
      first : second : _ -> do
        Spec.assertEqWith s "first" first (Just Nothing)
        Spec.assertBool s (maybe False Maybe.isJust second) "second"
      _ -> Spec.assertFailure s "fewer than two lines"

  Spec.it s "an answer naming what the prompt does not offer is asked again" $ do
    gs <- started s registry
    let strayed = State.execState (Driver.drive [observer] (Protocol.over trusting (exchanging stray) chance) 10000 (pure . (>= 3) . (TurnRegisters.turnNumber . GameState.turn)) gs) []
        discards = [Request.problem request | Just request <- fmap (requestOf . snd) (linesWithState strayed), Question.kind (Request.question request) == Text.pack "ChooseDiscard"]
    case discards of
      first : second : _ -> do
        Spec.assertEqWith s "first" first Nothing
        Spec.assertBool s (Maybe.isJust second) "second"
      _ -> Spec.assertFailure s "fewer than two discard lines"

  Spec.it s "CR 734.1 a seat's view says why its cast was reversed, until it chooses its next action" $ do
    gs <- started s registry
    bears <- S.printingOf s registry "Grizzly Bears"
    forest <- S.printingOf s registry "Forest"
    let (_, g1) = S.addPermanent forest S.alice gs
        (_, g2) = S.addPermanent forest S.alice g1
        board = snd (S.addHandCard bears S.alice g2)
        logged = State.execState (Driver.drive [observer] (Protocol.over trusting (exchanging stopsOnce) chance) 10000 (pure . (>= 4) . (TurnRegisters.turnNumber . GameState.turn)) board) []
        sent = fmap snd (linesWithState logged)
        afterStop = drop 1 (dropWhile (not . List.isInfixOf "\"kind\":\"ChooseManaSource\"") sent)
        undone = [PlayerView.undone view | Just request <- fmap requestOf afterStop, Question.seat (Request.question request) == S.alice, Question.kind (Request.question request) == Text.pack "ChooseAction", Just view <- [Request.view request]]
    case undone of
      next : later -> do
        Spec.assertEqWith s "the next priority says the cost was not paid" next (Just Undoing.UnpaidCost)
        Spec.assertBool s (not (null later)) "alice is asked again"
        Spec.assertEqWith s "and no later one says anything" (filter Maybe.isJust later) []
      [] -> Spec.assertFailure s "alice never stopped a payment"

  Spec.it s "CR 205.3e a subtype of the wrong kind is refused and asked again" $ do
    gs <- started s registry
    let oid = ObjectId.MkObjectId 1
        creature = Prompt.ChooseSubtype (Decider.MkDecider S.alice) S.alice oid SubtypeFamily.CreatureType Nothing
        basicLand = Prompt.ChooseSubtype (Decider.MkDecider S.alice) S.alice oid SubtypeFamily.BasicLandType Nothing
        replies = [Protocol.encodeAnswer creature Subtype.Island, Protocol.encodeAnswer creature Subtype.Goblin, Protocol.encodeAnswer basicLand Subtype.Goblin, Protocol.encodeAnswer basicLand Subtype.Swamp]
        ((chosen, _), sent) = overTheLine replies gs ((,) <$> Game.choose creature <*> Game.choose basicLand)
        refused = Just (Text.pack "not an answer the prompt offers")
    Spec.assertEqWith s "CR 205.3m creature type problems" (problemsOf "ChooseCreatureType" sent) [Nothing, refused]
    Spec.assertEqWith s "CR 305.6 basic land type problems" (problemsOf "ChooseBasicLandType" sent) [Nothing, refused]
    Spec.assertEqWith s "the second answers stand" chosen (Subtype.Goblin, Subtype.Swamp)

  Spec.it s "CR 601.2h counters removed past the cost are refused and asked again" $ do
    gs <- started s registry
    bears <- S.printingOf s registry "Grizzly Bears"
    sages <- S.printingOf s registry "Novijen Sages"
    let (b, g1) = S.addPermanent bears S.alice gs
        (n, g2) = S.addPermanent sages S.alice g1
        board = S.addCounter CounterKind.PlusOnePlusOne 4 n (S.addCounter CounterKind.PlusOnePlusOne 2 b g2)
        removal = Prompt.ChooseCounterRemovalAmong (Decider.MkDecider S.alice) S.alice n 2 (Map.fromList [(b, 2), (n, 4)])
        replies = fmap (Protocol.encodeAnswer removal . Map.singleton n) [3, 2]
        ((chosen, _), sent) = overTheLine replies board (Game.choose removal)
    Spec.assertEqWith s "problems" (problemsOf "ChooseCounterRemovalAmong" sent) [Nothing, Just (Text.pack "amounts outside the bounds the prompt states")]
    Spec.assertEqWith s "the second answer stands" chosen (Map.singleton n 2)

  Spec.it s "CR 509.1b an illegal block is refused naming the attacker, and asked again however often it is repeated" $ do
    drake <- S.printingOf s registry "Wind Drake"
    piker <- S.printingOf s registry "Goblin Piker"
    barbarians <- S.printingOf s registry "Balduvian Barbarians"
    -- The Piker gives the Barbarians a legal block, so bob is asked at all.
    case S.combatBoardOf [drake, piker] [barbarians] of
      (gs, [flier, walker], [blocker]) -> do
        let blocks = Protocol.encodeAnswer (Prompt.DeclareBlockers (Decider.MkDecider S.bob) S.bob [blocker] [flier, walker])
            illegal = blocks (Map.singleton blocker (Set.singleton flier))
            replies = [Protocol.encodeAnswer (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [flier, walker]) [flier, walker], illegal, illegal, blocks Map.empty]
            (sent, after) = declaring replies gs
            refused = Just (Text.pack "the declaration of blockers is illegal: Balduvian Barbarians can't block Wind Drake")
        Spec.assertEqWith s "problems" (problemsOf "DeclareBlockers" sent) [Nothing, refused, refused]
        Spec.assertEqWith s "the drake and the Piker attack" (Map.keys (Combat.Type.attackers (TurnRegisters.combat (GameState.turn after)))) [flier, walker]
        Spec.assertEqWith s "and are unblocked" (Combat.Type.blockers (TurnRegisters.combat (GameState.turn after))) Map.empty
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"

  Spec.it s "CR 508.1c an illegal attack is refused, and asked again however often it is repeated" $ do
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    bears <- S.printingOf s registry "Grizzly Bears"
    -- The Bears give the Construct company in some legal declaration, so alice
    -- is asked at all.
    case S.combatBoardOf [bondedConstruct, bears] [] of
      (gs, [construct, bear], []) -> do
        let attacks = Protocol.encodeAnswer (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [construct, bear])
            (sent, after) = declaring [attacks [construct], attacks [construct], attacks []] gs
            refused = Just (Text.pack "the declaration of attackers is illegal")
        Spec.assertEqWith s "problems" (problemsOf "DeclareAttackers" sent) [Nothing, refused, refused]
        Spec.assertEqWith s "nothing attacks" (Combat.Type.attackers (TurnRegisters.combat (GameState.turn after))) Map.empty
      _ -> Spec.assertFailure s "fixture should have two creatures"

  Spec.it s "CR 508.1b-c with several attack targets, an attack no announcement makes legal is refused, and one that some does is asked where each attacks" $ do
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    bears <- S.printingOf s registry "Grizzly Bears"
    jace <- S.printingOf s registry "Jace Beleren"
    case S.combatBoardOf [bondedConstruct, bears] [jace] of
      (gs, [construct, bear], [jaceId]) -> do
        let board = S.addCounter CounterKind.Loyalty 3 jaceId gs
            attacks = Protocol.encodeAnswer (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [construct, bear])
            at oid = Protocol.encodeAnswer (Prompt.ChooseAttackTarget (Decider.MkDecider S.alice) S.alice oid (AttackTarget.OfPlayer S.bob NonEmpty.:| [AttackTarget.OfPlaneswalker jaceId]))
            -- Answered by kind, so a line that wrongly lets the lone Construct
            -- through still ends in a declaration rather than out of replies.
            (sent, after) =
              declaringByKind
                [ ("DeclareAttackers", ([attacks [construct], attacks [construct], attacks [construct, bear]], attacks [])),
                  ("ChooseAttackTarget", ([at construct (AttackTarget.OfPlaneswalker jaceId), at bear (AttackTarget.OfPlayer S.bob)], at bear (AttackTarget.OfPlayer S.bob)))
                ]
                board
            refused = Just (Text.pack "the declaration of attackers is illegal")
        Spec.assertEqWith s "the targets" (Combat.declarableTargets board) [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlaneswalker jaceId]
        Spec.assertEqWith s "problems" (problemsOf "DeclareAttackers" sent) [Nothing, refused, refused]
        Spec.assertEqWith s "each attacks where announced" (Combat.Type.attackers (TurnRegisters.combat (GameState.turn after))) (Map.fromList [(construct, AttackTarget.OfPlaneswalker jaceId), (bear, AttackTarget.OfPlayer S.bob)])
      _ -> Spec.assertFailure s "fixture should give alice two creatures and bob a Jace"

  Spec.it s "CR 508.1b-d an announcement that would make the declaration illegal is never offered, so nothing is rewound" $ do
    siren <- S.printingOf s registry "Alluring Siren"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    case S.combatBoardOf [piker, centaur] [jace, siren] of
      (gs, [lured, free], [jaceId, sirenId]) | ability : _ <- Face.activatedAbilities (S.combinedFace siren) -> do
        let board = S.addCounter CounterKind.Loyalty 3 jaceId gs
            ready = board {GameState.turn = (GameState.turn board) {TurnRegisters.priority = Just S.bob}}
            -- Alluring Siren's "attacks you this turn if able" on the Piker.
            activated = snd (Engine.runGamePure (aimingAt lured) (snd (Engine.runGamePure (aimingAt lured) ready (Activate.activateAbility S.bob sirenId ability))) Stack.resolveTop)
            attacks = Protocol.encodeAnswer (Prompt.DeclareAttackers (Decider.MkDecider S.alice) S.alice [lured, free])
            atJace = Protocol.encodeAnswer (Prompt.ChooseAttackTarget (Decider.MkDecider S.alice) S.alice free (AttackTarget.OfPlayer S.bob NonEmpty.:| [AttackTarget.OfPlaneswalker jaceId])) (AttackTarget.OfPlaneswalker jaceId)
            noBlocks = Protocol.encodeAnswer (Prompt.DeclareBlockers (Decider.MkDecider S.bob) S.bob [] []) Map.empty
            -- Every announcement asked is answered Jace, so a lured creature
            -- offered Jace takes it and the declaration is rewound.
            (sent, after) =
              declaringByKind
                [ ("DeclareAttackers", ([], attacks [lured, free])),
                  ("ChooseAttackTarget", ([], atJace)),
                  ("DeclareBlockers", ([], noBlocks))
                ]
                activated
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(lured, AttackTarget.OfPlaneswalker jaceId), (free, AttackTarget.OfPlaneswalker jaceId)] activated)) "CR 508.1d: the lured Piker at Jace is illegal"
        Spec.assertEqWith s "the lured Piker attacks bob, the Centaur Jace" (Combat.Type.attackers (TurnRegisters.combat (GameState.turn after))) (Map.fromList [(lured, AttackTarget.OfPlayer S.bob), (free, AttackTarget.OfPlaneswalker jaceId)])
        Spec.assertEqWith s "asked where to attack once, for the free Centaur" (problemsOf "ChooseAttackTarget" sent) [Nothing]
        Spec.assertEqWith s "and the declaration once, never rewound" (problemsOf "DeclareAttackers" sent) [Nothing]
      _ -> Spec.assertFailure s "fixture should give alice two creatures and bob a Jace and a Siren"

-- CR 601.2c: aims every target at `oid`, answering the rest as identityAnswer.
aimingAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Declares attackers then blockers over the line, answering each line sent
-- with the next of `replies`; the lines sent and the state after.
declaring :: [R.Reply] -> GameState.GameState -> ([String], GameState.GameState)
declaring replies gs =
  let ((_, after), sent) = overTheLine replies gs (Combat.declareAttackers S.manaPerformer S.alice >> Combat.declareBlockers S.manaPerformer)
   in (sent, after)

-- declaring, answering each line from the queue its prompt's kind names, then
-- with that kind's default once the queue is spent.
declaringByKind :: [(String, ([R.Reply], R.Reply))] -> GameState.GameState -> ([String], GameState.GameState)
declaringByKind queues gs =
  let send line = State.state $ \(out, rest) ->
        let kind = maybe "" (Text.unpack . Question.kind . Request.question) (requestOf (Text.unpack line))
            (reply, rest') = case Map.lookup kind rest of
              Just (next : more, spent) -> (next, Map.insert kind (more, spent) rest)
              Just ([], spent) -> (spent, rest)
              Nothing -> error ("no queue for " <> Text.unpack line)
         in (Common.render (Codec.Reply.toValue reply), (out <> [Text.unpack line], rest'))
      fallback :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.State ([String], Map.Map String ([R.Reply], R.Reply)))
      fallback = Answerer.MkAnswerer (pure . Replay.defaultAnswer . Asked.prompt)
      game = Combat.declareAttackers S.manaPerformer S.alice >> Combat.declareBlockers S.manaPerformer
      ((_, after), (sent, _)) = State.runState (Engine.runGameAsked (Answerer.unwrap (Protocol.over trusting send fallback)) gs game) ([], Map.fromList queues)
   in (sent, after)

-- Runs `game` over the line, answering each line sent with the next of
-- `replies`; its result and state after, and the lines sent.
overTheLine :: [R.Reply] -> GameState.GameState -> Game.Type.Game a -> ((a, GameState.GameState), [String])
overTheLine replies gs game =
  let send line = State.state $ \(out, rest) -> case rest of
        reply : more -> (Common.render (Codec.Reply.toValue reply), (out <> [Text.unpack line], more))
        [] -> error ("no reply left for " <> Text.unpack line)
      fallback :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.State ([String], [R.Reply]))
      fallback = Answerer.MkAnswerer (pure . Replay.defaultAnswer . Asked.prompt)
      (ran, (sent, _)) = State.runState (Engine.runGameAsked (Answerer.unwrap (Protocol.over trusting send fallback)) gs game) ([], replies)
   in (ran, sent)

-- The problem each line of this kind was sent with.
problemsOf :: String -> [String] -> [Maybe Text.Text]
problemsOf kind sent = [Request.problem request | Just request <- fmap requestOf sent, Question.kind (Request.question request) == Text.pack kind]

-- Every prompt's state before its answer, and every line sent, in order.
type Log = [Either GameState.GameState String]

observer :: Observer.Observer (State.State Log)
observer =
  Observer.MkObserver
    { Observer.before = \asked -> State.modify' (<> [Left (Asked.game asked)]),
      Observer.after = \_ _ -> pure ()
    }

-- No answer the host refuses: every one stands.
trusting :: (Applicative m) => Asked.Asked GameState.GameState r -> r -> m (Either Text.Text r)
trusting _ = pure . Right

-- Sends a line to a client that answers from that line alone.
exchanging :: (Text.Text -> State.State Log Text.Text) -> Text.Text -> State.State Log Text.Text
exchanging answer line = State.modify' (<> [Right (Text.unpack line)]) >> answer line

-- Chance and the interpreter's prompts, answered as a recording would be.
chance :: Answerer.Answerer (Asked.Asked GameState.GameState) (State.State Log)
chance = Answerer.MkAnswerer (pure . Replay.defaultAnswer . Asked.prompt)

-- Answers "nonsense" to the first line it is sent, then as `client` does.
once :: Text.Text -> State.State Log Text.Text
once line = do
  sent <- State.gets (length . Either.rights)
  if sent == 1 then pure (Text.pack "nonsense") else client line

-- Discards a card that does not exist the first time it is asked to discard,
-- then answers as `client` does.
stray :: Text.Text -> State.State Log Text.Text
stray line = do
  asked <- State.gets (length . filter (Text.isInfixOf (Text.pack "\"kind\":\"ChooseDiscard\"") . Text.pack) . Either.rights)
  if asked == 1 && Text.isInfixOf (Text.pack "\"kind\":\"ChooseDiscard\"") line then pure (Text.pack "[\"$9999\"]") else client line

-- Casts the first spell offered and stops paying for it, the first time; then
-- answers as `client` does.
stopsOnce :: Text.Text -> State.State Log Text.Text
stopsOnce line = do
  stopped <- State.gets (any (List.isInfixOf "\"kind\":\"ChooseManaSource\"") . Either.rights)
  case fmap Request.question (requestOf (Text.unpack line)) of
    Just question
      | Question.kind question == Text.pack "ChooseManaSource" -> pure (Text.pack "null")
      | Question.kind question == Text.pack "ChooseAction",
        not stopped,
        R.Array actions <- Question.offer question,
        Just i <- List.findIndex ((== Just (R.Text (Text.pack "Cast"))) . field "action") actions ->
          pure (Text.pack (show i))
    _ -> client line

-- Passes, keeps, declines and discards the first cards offered, reading only
-- the line.
client :: Text.Text -> State.State Log Text.Text
client line = case requestOf (Text.unpack line) of
  Nothing -> error ("unreadable line: " <> Text.unpack line)
  Just request ->
    let question = Request.question request
        offer = Question.offer question
     in pure . Common.render . Codec.Reply.toValue $ case Text.unpack (Question.kind question) of
          "Concede" -> R.Text (Text.pack "Continues")
          "DeclareMulligan" -> R.Text (Text.pack "Keep")
          "ChooseAction" -> case offer of
            R.Array actions -> maybe R.Null (R.Number . toInteger) (List.elemIndex (R.Object [(Text.pack "action", R.Text (Text.pack "Pass"))]) actions)
            _ -> R.Null
          "ChooseDiscard" -> case (field "from" offer, field "count" offer) of
            (Just (R.Array from), Just (R.Number count)) -> R.Array (List.genericTake count from)
            _ -> R.Null
          kind -> error ("unexpected prompt: " <> kind)

field :: String -> R.Reply -> Maybe R.Reply
field key reply = case reply of
  R.Object pairs -> lookup (Text.pack key) pairs
  _ -> Nothing

arrayOf :: R.Reply -> Maybe [R.Reply]
arrayOf reply = case reply of
  R.Array xs -> Just xs
  _ -> Nothing

requestOf :: String -> Maybe Request.Request
requestOf line = Either.either (const Nothing) Just (Common.parse (Text.pack line) >>= Codec.decode Codec.Request.codec)

-- Each line sent, with the state of the game whose prompt it carries.
linesWithState :: Log -> [(GameState.GameState, String)]
linesWithState = go Nothing
  where
    go current entries = case entries of
      [] -> []
      Left gs : rest -> go (Just gs) rest
      Right line : rest -> maybe id (\gs -> ((gs, line) :)) current (go current rest)

-- A player's hand and library.
hiddenOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
hiddenOf pid gs = Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Library pid gs

-- Every name a seat may see: its own hand and every public zone.
seenBy :: PlayerId.PlayerId -> GameState.GameState -> Set.Set CardName.CardName
seenBy pid gs =
  let public = [Game.zoneMembers zone owner gs | owner <- [S.alice, S.bob], zone <- [Zone.Battlefield, Zone.Graveyard, Zone.Exile, Zone.Stack]]
   in Set.unions (fmap (`Projection.namesOf` gs) (Game.zoneMembers Zone.Hand pid gs <> concat public))

redGreen :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
redGreen s registry = do
  red <- Cards.redDeck (S.printingOf s registry)
  green <- Cards.greenDeck (S.printingOf s registry)
  pure ((S.alice, red) NonEmpty.:| [(S.bob, green)])

-- A game after setup: alice red, bob green, hands drawn.
started :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
started s registry = do
  matchup <- redGreen s registry
  pure (snd (Engine.runGamePure S.castAnswer (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame (Resolve.performHandAction Engine.playSubgame) matchup)))
