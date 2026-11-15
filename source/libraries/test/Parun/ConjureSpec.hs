{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Alchemy's conjure keyword action -- Parun.Types.Conjure,
-- Parun.Types.ConjureDestination and Parun.Types.CardSelection,
-- Parun.Engine.Resolve's Effect.Conjure arm, and Parun.Engine.Event's conjure and
-- mintCard (the mint CR 400.11c's wish shares, which Parun.OutsideTheGameSpec
-- drives from the other side).
--
-- Gameplay-level throughout: the first two cases put a printed Emporium
-- Thopterist on the battlefield and begin its controller's upkeep so the printed
-- trigger fires and resolves; the next two conjure into a graveyard, the second
-- under a printed Planar Void; the fifth enters a printed Foundry Groundbreaker,
-- whose conjure STATES the status its arrivals take; the sixth attacks with a
-- printed Kari Zev, Crew of Two; the next run of cases cast a printed Sinister
-- Reflections, whose conjure names an object already in the game rather than
-- writing its card out; the next four enter a printed Fear of Change, whose
-- conjure picks from the Oracle card reference an interpreter holds; the next
-- kills a printed Gyox, Brutal Carnivora's oiled creature; the last five discard
-- a printed Calim, Djinn Emperor and enter a printed Mine Security, whose
-- conjures state how deep into a library the card lands. Their scenario
-- siblings are under data/scenarios/conjure/.
--
-- The first two CAST what the conjure created, which is the point -- conjure
-- creates a CARD and not CR 111.1's token, and a token in a hand, a library or a
-- graveyard would be swept up by CR 111.7 before any cast. The BATTLEFIELD case
-- proves the entry rather than the cardness: rule 111.7 sweeps up nothing there,
-- so no board of that shape tells a conjured card from a token. The fourth is
-- the graveyard arrival's SHAPE rather than its cardness, read off the Void that
-- watches the graveyard.
--
-- The DUPLICATE cases are the other axis: three read the abilities of a
-- duplicate of a Clone off a graveyard and a hand (CR 113.6).
module Parun.ConjureSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural
import qualified Parun.Engine.Action as Action
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Cast as Cast
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Interpreter as Interpreter
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CounterKind as CounterKind
import qualified Parun.Types.CounterName as CounterName
import qualified Parun.Types.DiscardCause as DiscardCause
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.PaymentDecision as PaymentDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Regenerability as Regenerability
import qualified Parun.Types.Staged as Staged
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- Parun.CounterspellSpec's bitterblossomChain, which is the shape both cases
-- want: record the step's beginning, settle the trigger onto the stack, then run
-- the priority loop so it resolves.
upkeepOf :: GameState.GameState -> GameState.GameState
upkeepOf gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          ( gs
              { GameState.turn =
                  (GameState.turn gs)
                    { TurnRegisters.phase = upkeep,
                      TurnRegisters.activePlayer = S.alice
                    }
              }
          )
   in settleTriggers begun

-- upkeepOf's tail on its own: settle whatever is pending onto the stack (CR
-- 603.3b) and run the priority loop until it has all resolved. The two cases
-- below need it without a step beginning, their triggers firing off an entry and
-- off a cast.
settleTriggers :: GameState.GameState -> GameState.GameState
settleTriggers gs =
  let onStack = S.runPure S.identityAnswer gs Engine.settleForPriority
   in S.runPure S.identityAnswer onStack Engine.priorityLoop

ornithopter :: CardName.CardName
ornithopter = CardName.MkCardName (Text.pack "Ornithopter")

lightningBolt :: CardName.CardName
lightningBolt = CardName.MkCardName (Text.pack "Lightning Bolt")

thinkTwice :: CardName.CardName
thinkTwice = CardName.MkCardName (Text.pack "Think Twice")

mishrasFoundry :: CardName.CardName
mishrasFoundry = CardName.MkCardName (Text.pack "Mishra's Foundry")

islandName :: CardName.CardName
islandName = CardName.MkCardName (Text.pack "Island")

namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs = fmap (\oid -> S.soleFaceName oid gs) (Game.zoneMembers zone S.alice gs)

namedIn :: CardName.CardName -> Zone.Zone -> GameState.GameState -> [ObjectId.ObjectId]
namedIn name zone gs = filter (\oid -> S.soleFaceName oid gs == name) (Game.zoneMembers zone S.alice gs)

-- Whole steps until `phase` is current, bounded like Parun.Support's combatGame.
runStepsUntil :: Phase.Phase -> Game.Type.Game ()
runStepsUntil phase =
  let go n = do
        gs <- State.get
        Monad.unless (n <= (0 :: Int) || TurnRegisters.phase (GameState.turn gs) == phase || Maybe.isJust (GameState.result gs)) (Engine.runStep >> go (n - 1))
   in go 24

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Parun.Conjure" $ do
  -- Emporium Thopterist ({1}{U} Creature -- Vedalken Artificer, "Thopters you
  -- control get +2/+0. At the beginning of your upkeep, conjure a card named
  -- Ornithopter into your hand.").
  --
  -- The Thopterist's own static ability is what makes the first assertion
  -- discriminating: Ornithopter is printed 0/2, so a 2/2 on the battlefield is
  -- the conjured card being a Thopter its controller controls, seen by layer 7c
  -- -- not merely something with the right name.
  Spec.it s "conjure puts a castable card named Ornithopter into the conjuring player's hand" $ do
    island <- S.printingOf s registry "Island"
    thopterist <- S.printingOf s registry "Emporium Thopterist"
    let (_, board) = S.addPermanent thopterist S.alice (S.landsInPlay island 1)
        conjured = upkeepOf board
        inHand = namedIn ornithopter Zone.Hand conjured
        -- CR 302.1: a creature card is cast from a hand during a main phase with
        -- the stack empty, so `castable` below is asked there rather than in the
        -- upkeep the card arrived in.
        main_ =
          conjured
            { GameState.turn =
                (GameState.turn conjured)
                  { TurnRegisters.phase = Phase.PrecombatMain
                  }
            }
        cast_ = case inHand of
          oid : _ -> S.runPure S.identityAnswer main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
    Spec.assertEqWith
      s
      "the conjured card was cast and is a 2/2 Thopter on the battlefield"
      (fmap (\oid -> S.powerToughnessOf oid cast_) (namedIn ornithopter Zone.Battlefield cast_))
      [Just (2, 2)]
    Spec.assertEqWith
      s
      "it was a card its owner could cast"
      (fmap (\oid -> S.castable S.alice oid main_) inHand)
      [True]
    Spec.assertEqWith
      s
      "exactly one Ornithopter reached alice's hand"
      (length inHand)
      1
  -- Conjure creates the card out of nothing, so nothing is SPENT -- the half
  -- Parun.Engine.Event.bringIn adds over the shared mint, where CR
  -- 400.11b keeps a wish from finding the same copy twice. Two upkeeps, two
  -- distinct Ornithopters.
  Spec.it s "a second upkeep conjures a second Ornithopter" $ do
    island <- S.printingOf s registry "Island"
    thopterist <- S.printingOf s registry "Emporium Thopterist"
    let (_, board) = S.addPermanent thopterist S.alice (S.landsInPlay island 1)
        twice = upkeepOf (upkeepOf board)
    Spec.assertEqWith
      s
      "two Ornithopters, and they are two objects"
      (length (namedIn ornithopter Zone.Hand twice))
      2

  -- Shellfish Scholar ({1}{U} Creature -- Rat Wizard, 2/2, "Whenever Shellfish
  -- Scholar or another Rat you control enters, conjure a card named Think Twice
  -- into your graveyard."). The Scholar is itself a Rat alice controls, so the
  -- one filter covers both halves of the printed sentence (CR 603.6a).
  --
  -- Think Twice ({1}{U} Instant, "Draw a card." / "Flashback {2}{U}") is what
  -- makes the graveyard arrival discriminating: CR 702.34a casts it FROM the
  -- graveyard, which a token could not be -- CR 111.7 would have swept it up as
  -- a state-based action long before -- and CR 702.34a's exile then moves it
  -- somewhere no other clause here could put it.
  --
  -- The Scholar's other printed line -- "Threshold -- {T}: Spells you cast from
  -- your graveyard this turn cost {2} less to cast. Activate only if seven or
  -- more cards are in your graveyard." -- is carried too, and this case does not
  -- reach it: three Islands pay Think Twice's {2}{U} flashback with no reduction,
  -- and the Scholar is never activated here. Parun.ActivateSpec's
  -- PrintedActivationThresholdReduction group is what proves that half. The
  -- reduction's filter is Filter.WasCastFrom, proved by Parun.PlayerEffectSpec's
  -- Patrician Geist group.
  Spec.it s "conjure puts a card into the conjuring player's graveyard, castable out of it" $ do
    islandPrinting <- S.printingOf s registry "Island"
    scholar <- S.printingOf s registry "Shellfish Scholar"
    let (_, entered) = S.entersWithTrigger scholar S.alice (S.landsInPlay islandPrinting 3)
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, stocked) = S.addLibraryCard islandPrinting S.alice entered
        conjured = settleTriggers stocked
        inYard = namedIn thinkTwice Zone.Graveyard conjured
        -- CR 117.1a: an instant needs only priority, so the main phase is where
        -- the cases above cast from rather than something this one requires.
        main_ =
          conjured
            { GameState.turn =
                (GameState.turn conjured)
                  { TurnRegisters.phase = Phase.PrecombatMain
                  }
            }
        flashedBack = case inYard of
          oid : _ -> S.runPure S.identityAnswer main_ (S.cast S.alice oid >> Stack.resolveTop)
          [] -> main_
    -- The hand is read BY NAME rather than by size: a Think Twice that reached
    -- the hand instead of the graveyard would leave the size right and the name
    -- wrong.
    Spec.assertEqWith
      s
      "it was cast out of the graveyard for its flashback cost, so alice drew the Island, and CR 702.34a exiled it"
      (namesIn Zone.Hand flashedBack, namesIn Zone.Exile flashedBack)
      ([islandName], [thinkTwice])
    Spec.assertEqWith
      s
      "exactly one Think Twice reached alice's graveyard and none reached her hand"
      (length inYard, length (namedIn thinkTwice Zone.Hand conjured))
      (1, 0)
  -- The same conjure watched by a bystander, which is what makes the arrival's
  -- SHAPE observable: Planar Void ({B} Enchantment, "Whenever another card is put
  -- into a graveyard from anywhere, exile that card"), Oracle text verified on
  -- Scryfall 2026-09-10. Parun.ZoneTriggerSpec's Planar Void group is where it
  -- fires on a card put into a graveyard from a HAND.
  --
  -- One board, two Think Twices, differing in exactly one thing -- how each
  -- reached the graveyard. The conjured one is materialized there in no zone
  -- change, so CR 603.6's zone-change trigger has no event to match (CR 400.6)
  -- and the Void leaves it alone; the cast one is put there from the stack and
  -- the Void exiles it. The second assertion is what keeps the first from
  -- passing because the Void is dead on this board.
  Spec.it s "CR 603.6 a conjure into a graveyard is no zone change, so a graveyard trigger does not see it" $ do
    islandPrinting <- S.printingOf s registry "Island"
    scholar <- S.printingOf s registry "Shellfish Scholar"
    planarVoid <- S.printingOf s registry "Planar Void"
    twicePrinting <- S.printingOf s registry "Think Twice"
    let (_, withVoid) = S.addPermanent planarVoid S.alice (S.landsInPlay islandPrinting 3)
        (handCard, withHand) = S.addHandCard twicePrinting S.alice withVoid
        -- Think Twice draws, and CR 104.3c would lose alice the game out from
        -- under the assertions on an empty library.
        (_, stocked) = S.addLibraryCard islandPrinting S.alice withHand
        (_, entered) = S.entersWithTrigger scholar S.alice stocked
        conjured = settleTriggers entered
        inYard = namedIn thinkTwice Zone.Graveyard conjured
        -- CR 304.1: an instant needs only priority, and the cast is what puts the
        -- SECOND Think Twice into the graveyard out of a zone the card was in.
        main_ =
          conjured
            { GameState.turn =
                (GameState.turn conjured)
                  { TurnRegisters.phase = Phase.PrecombatMain
                  }
            }
        cast_ = settleTriggers (S.runPure S.identityAnswer main_ (S.cast S.alice handCard >> Stack.resolveTop))
    Spec.assertEqWith
      s
      "CR 603.6 the Void saw no arrival, so the conjured Think Twice stayed in the graveyard and nothing was exiled"
      (length inYard, namesIn Zone.Exile conjured)
      (1, [])
    Spec.assertEqWith
      s
      "CR 603.6 the CAST one was put into the graveyard from the stack, and the same Void exiled that one alone"
      (namedIn thinkTwice Zone.Graveyard cast_, length (namedIn thinkTwice Zone.Exile cast_))
      (inYard, 1)
  -- Foundry Groundbreaker ({3}{G} Creature -- Human Artificer, 3/4, "When
  -- Foundry Groundbreaker enters, sacrifice a land. Then conjure two cards named
  -- Mishra's Foundry onto the battlefield tapped."), Oracle text verified on
  -- Scryfall 2026-09-21. The same entry the Lam case drives, with the one thing
  -- that arm could not say until now: CR 110.5b's status, STATED by the sentence.
  --
  -- The gameplay assertion is the mana, not the flag. A land is tapped for mana
  -- (CR 605.1a), so two Foundries that arrived untapped would be two more
  -- Action.ActivateManaAbility offers in alice's menu; the surviving Island is
  -- what says the menu is read at all and that the board is one where a land CAN
  -- be tapped. The status read off the objects is beside it rather than instead
  -- of it.
  --
  -- TWO conjured, which is the count and the status together: a batch is minted
  -- whole before any member enters, so a status applied to the arrival rather
  -- than to the mint could reach one of them and not the other.
  --
  -- The sacrifice is the rest of the printed trigger, and alice is left one
  -- Island of the two: an effect list that stopped at the conjure would leave
  -- both.
  Spec.it s "CR 110.5b a conjure that states tapped puts the card onto the battlefield tapped" $ do
    islandPrinting <- S.printingOf s registry "Island"
    groundbreaker <- S.printingOf s registry "Foundry Groundbreaker"
    let board0 = S.landsInPlay islandPrinting 2
        (_, entered) = S.entersWithTrigger groundbreaker S.alice board0
        final =
          (settleTriggers entered)
            { GameState.turn =
                (GameState.turn (settleTriggers entered))
                  { TurnRegisters.priority = Just S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain
                  }
            }
        foundries = namedIn mishrasFoundry Zone.Battlefield final
        islands = namedIn islandName Zone.Battlefield final
        manaOffers = Maybe.mapMaybe manaSource (Action.legalActions S.alice final)
    Spec.assertEqWith
      s
      "CR 605.1a neither conjured Mishra's Foundry can be tapped for mana, where the Island alice kept can"
      (filter (`elem` foundries) manaOffers, filter (`elem` islands) manaOffers)
      ([], islands)
    Spec.assertEqWith
      s
      "both of them arrived tapped"
      (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid final)) foundries)
      [Just TapState.Tapped, Just TapState.Tapped]
    Spec.assertEqWith
      s
      "the trigger's own sacrifice ran, so one of alice's two Islands is gone"
      (length islands)
      1
  -- Kari Zev's conjured Ragavan's own trigger: "Whenever Ragavan deals combat damage to
  -- a player, create a Treasure token and exile the top card of that player's
  -- library. Until end of turn, you may cast that card." CR 601.3 / 305.9: the
  -- permission's verb is Cast, so a nonland card is castable (the Treasure pays
  -- its {R}) and a land is not playable. The pair differs only in bob's top card.
  Spec.it s "CR 305.9 Ragavan's exiled card may be cast, and an exiled land may not be played" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let setup = S.duel S.beginningOfCombat [S.settled "kari" "Kari Zev, Crew of Two"] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "kari"])]
        exiledAfter top = do
          built <- S.buildBoardOrFail s registry setup
          let stocked = built {Staged.state = snd (S.addLibraryCard top S.bob (Staged.state built))}
          (_, gs) <- S.runScriptOrFail s script stocked (runStepsUntil S.postcombatMain)
          pure
            ( Game.zoneMembers Zone.Exile S.bob gs,
              gs
                { GameState.turn =
                    (GameState.turn gs)
                      { TurnRegisters.priority = Just S.alice
                      }
                }
            )
        playsLand oid gs = any (\action -> case action of Action.Play played _ -> played == oid; _ -> False) (Action.legalActions S.alice gs)
    (bolts, boltGame) <- exiledAfter bolt
    (mountains, mountainGame) <- exiledAfter mountain
    Spec.assertEqWith
      s
      "CR 601.3 the exiled Lightning Bolt is castable and the exiled Mountain is not playable"
      (fmap (\oid -> castOffered oid lightningBolt boltGame) bolts, fmap (`playsLand` mountainGame) mountains)
      ([True], [False])
  -- CR 707.2 / 113.6b: the abilities a card has off the battlefield are its
  -- copiable values' too. A Clone copying bob's Anger is duplicated and the
  -- duplicate discarded, so alice's graveyard holds a card printed Clone whose
  -- rules text is Anger's "as long as this card is in your graveyard and you
  -- control a Mountain, creatures you control have haste". The pair differs only
  -- in alice's Mountain; her Goblin Piker is the creature that reads it.
  Spec.it s "CR 707.2/113.6b a duplicate of a Clone of Anger gives haste from the graveyard" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    anger <- S.printingOf s registry "Anger"
    let (angerId, board) = S.addPermanent anger S.bob (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections angerId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) -> do
        let (pikerId, withPiker) = S.addPermanent piker S.alice conjured
            discarded = S.runPure S.identityAnswer withPiker (Event.discard (pure ()) DiscardCause.Ordinary S.alice duplicate)
            hasty = Projection.hasKeyword Keyword.Haste pikerId
        Spec.assertEqWith
          s
          "CR 113.6b the Piker has haste only beside a Mountain: (no Mountain, a Mountain)"
          (hasty discarded, hasty (S.landsFor mountain S.alice 1 discarded))
          (False, True)
        Spec.assertEqWith s "the duplicate is the one card in alice's graveyard" (length (namedIn cloneName Zone.Graveyard discarded)) 1
  -- CR 707.2 / 113.6k: the triggered side of the same read. A Clone copying
  -- bob's Bloodghast is duplicated and the duplicate discarded, so alice's
  -- graveyard holds a card printed Clone whose rules text is Bloodghast's
  -- "Landfall -- whenever a land you control enters, you may return this card
  -- from your graveyard to the battlefield". alice plays a Forest; the trigger
  -- resolves and the duplicate returns, a second Clone printing on her
  -- battlefield.
  Spec.it s "CR 707.2/113.6k a duplicate of a Clone of Bloodghast returns on landfall" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    bloodghast <- S.printingOf s registry "Bloodghast"
    let (ghastId, board) = S.addPermanent bloodghast S.bob (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections ghastId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) -> do
        let discarded = S.runPure S.identityAnswer conjured (Event.discard (pure ()) DiscardCause.Ordinary S.alice duplicate)
            (forestId, withForest) = S.addHandCard forest S.alice discarded
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseOptional {} -> OptionalDecision.Exercises
              _ -> S.identityAnswer p
            played = S.runPure answer withForest (Cast.playLand (pure ()) True S.alice forestId Nothing)
            onStack = S.runPure answer played Engine.settleForPriority
            resolved = S.runPure answer onStack (Stack.resolveTop >> Engine.settleForPriority)
        Spec.assertEqWith
          s
          "CR 113.6k the duplicate returned: (Clone printings on alice's battlefield, in her graveyard)"
          (length (clonesOnBattlefield resolved), length (namedIn cloneName Zone.Graveyard resolved))
          (2, 0)
        Spec.assertEqWith s "the landfall trigger reached the stack" (length (TurnRegisters.stack (GameState.turn onStack))) 1
  -- CR 707.2 / 702.35a: the hand's side. A Clone copying bob's Arrogant Wurm
  -- (madness {2}{G}) is duplicated, so alice's hand holds a card printed Clone
  -- with the Wurm's madness, and discarding it discards it into exile.
  Spec.it s "CR 707.2/702.35a a duplicate of a Clone of Arrogant Wurm is discarded into exile" $ do
    island <- S.printingOf s registry "Island"
    clone <- S.printingOf s registry "Clone"
    reflections <- S.printingOf s registry "Sinister Reflections"
    wurm <- S.printingOf s registry "Arrogant Wurm"
    let (wurmId, board) = S.addPermanent wurm S.bob (S.landsInPlay island 2)
    case conjuredDuplicate clone reflections wurmId board of
      Nothing -> Spec.assertFailure s "the Clone left the battlefield unexpectedly"
      Just (duplicate, conjured) -> do
        let discarded = S.runPure S.identityAnswer conjured (Event.discard (pure ()) DiscardCause.Ordinary S.alice duplicate)
        Spec.assertEqWith
          s
          "CR 702.35a the duplicate went to exile, not the graveyard: (in exile, in the graveyard)"
          (length (namedIn cloneName Zone.Exile discarded), length (namedIn cloneName Zone.Graveyard discarded))
          (1, 0)
  -- Fear of Change ({G}{U} Enchantment Creature -- Nightmare, 2/3, "When this
  -- creature enters or dies, exile another creature you control. If you do,
  -- conjure a duplicate of a random creature card with mana value X onto the
  -- battlefield, where X is 2 plus the exiled creature's mana value."), Oracle
  -- text verified on Scryfall 2026-09-24. The REFERENCE pick (CR 108.1): no card
  -- is written out, and the candidates come off the interpreter's registry.
  --
  -- The reference is a fixture of four, so the filter has something to refuse
  -- on each axis: a Goblin Piker is a creature at the exiled Piker's own mana
  -- value, 2 rather than 2 plus 2, and Ancient Vendetta is mana value 4 but no
  -- creature. The answerer names the first of Goblin Piker, Ancient Vendetta and
  -- Giant Spider it is offered, so an offer the filter did not narrow conjures
  -- no Spider, and an engine that rolled the pick itself lands on the offer's
  -- head, Hill Giant.
  Spec.it s "a conjure from the reference offers the creature cards at 2 plus the exiled mana value, and conjures the one randomness named" $ do
    (final, offers) <- fearOfChangeRun s registry True False
    Spec.assertEqWith
      s
      "the Giant Spider randomness named entered the battlefield under alice"
      (length (namedIn giantSpider Zone.Battlefield final))
      1
    Spec.assertEqWith
      s
      "the other Goblin Piker is what the trigger exiled"
      (length (namedIn goblinPiker Zone.Exile final), length (namedIn goblinPiker Zone.Battlefield final))
      (1, 0)
    -- Supporting, and LAST: the offer, recorded off the prompt since the
    -- candidate list is not readable off the board.
    Spec.assertEqWith
      s
      "offered the mana value 4 creature cards of the reference, once"
      offers
      [[hillGiant, giantSpider]]
  -- The pair above with the one difference: no other creature to exile, so the
  -- "if you do" fails and nothing is conjured.
  Spec.it s "a conjure from the reference behind an exile that found nothing conjures nothing" $ do
    (final, _) <- fearOfChangeRun s registry False False
    Spec.assertEqWith
      s
      "nothing but Fear of Change is on alice's battlefield"
      (namesIn Zone.Battlefield final)
      [fearOfChange]
  -- The first case's board with a reference that LIES, offering all four cards;
  -- randomness names the Goblin Piker, which the filter does not admit. Filtered,
  -- not trusted: the conjure checks the card it is handed back and conjures
  -- nothing.
  Spec.it s "a conjure from the reference refuses a card the reference offered outside its filter" $ do
    (final, offers) <- fearOfChangeRun s registry True True
    Spec.assertEqWith
      s
      "the offered Goblin Piker did not enter, and nothing else did"
      (namesIn Zone.Battlefield final)
      [fearOfChange]
    Spec.assertEqWith
      s
      "the lying reference was what offered it"
      offers
      [[goblinPiker, hillGiant, ancientVendetta, giantSpider]]
  -- The first case's board over the suite's own FILE registry, beside a
  -- Llanowar Elves so X is 3, a mana value synthetic creature cards in
  -- data/cards/ share. Randomness names a synthetic wherever one is offered: a
  -- synthetic is a test fixture and no card of the Oracle card reference, so
  -- none is, and a real card enters.
  Spec.it s "a conjure from the file registry's reference never conjures a synthetic card" $ do
    fear <- S.printingOf s registry "Fear of Change"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (_, withElves) = S.addPermanent elves S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger fear S.alice withElves
        lifted = Registry.hoist Trans.lift registry
        synthetic name = Text.pack "Synthetic " `Text.isPrefixOf` CardName.unwrap name
        answer :: (Monad m) => Asked.Asked GameState.GameState r -> State.StateT [[CardName.CardName]] m r
        answer asked = case Asked.prompt asked of
          Prompt.RandomCard offered -> do
            State.modify' (NonEmpty.toList offered :)
            pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find synthetic (NonEmpty.toList offered)))
          p -> pure (S.identityAnswer p)
        run = do
          (_, settled) <- Engine.runGameAsked (Interpreter.lookingUpCards lifted (Trans.lift . S.brokenCorpus s) answer) entered Engine.settleForPriority
          Engine.runGameAsked (Interpreter.lookingUpCards lifted (Trans.lift . S.brokenCorpus s) answer) settled Engine.priorityLoop
    ((_, final), logged) <- State.runStateT run []
    let arrived = filter (`notElem` [fearOfChange, llanowarElves]) (namesIn Zone.Battlefield final)
    Spec.assertEqWith
      s
      "one creature card entered, and it is no synthetic"
      (length arrived, filter synthetic arrived)
      (1, [])
    Spec.assertEqWith
      s
      "the reference offered no synthetic"
      (concatMap (filter synthetic) logged, length logged)
      ([], 1)
  -- Gyox, Brutal Carnivora ({1}{B}{G} Legendary Creature -- Phyrexian Warlock
  -- 2/4, "... Whenever a nontoken creature you control with one or more oil
  -- counters on it dies, conjure X duplicates of it into exile, where X is the
  -- number of oil counters on it. Those duplicates perpetually get +X/+X. Then
  -- shuffle those duplicates into your library."), Oracle text verified on
  -- Scryfall 2026-10-12. The COUNT on the duplicate road, off the battlefield:
  -- two oil counters make two duplicates, not one per named object. alice's
  -- library starts empty, so everything in it came from the conjure.
  Spec.it s "a conjure of X duplicates into exile makes X of them, one per oil counter" $ do
    gyox <- S.printingOf s registry "Gyox, Brutal Carnivora"
    giant <- S.printingOf s registry "Hill Giant"
    let oil = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "oil"))
        (_, board0) = S.addPermanent gyox S.alice (Setup.emptyGame S.bothPlayers)
        (giantId, board1) = S.addPermanent giant S.alice board0
        oiled = S.addCounter oil 2 giantId board1
        final = settleTriggers (S.runPure S.identityAnswer oiled (Event.destroy (pure ()) Regenerability.Regenerable [giantId]))
    Spec.assertEqWith
      s
      "two Hill Giants were shuffled into alice's library"
      (length (namedIn hillGiant Zone.Library final))
      2
    Spec.assertEqWith
      s
      "and none is left in exile, the original in the graveyard"
      (namedIn hillGiant Zone.Exile final, length (namedIn hillGiant Zone.Graveyard final))
      ([], 1)

  -- Calim, Djinn Emperor ({3}{U}{U}{U} Legendary Creature -- Djinn Noble 5/6,
  -- flying, ward {2}, "When you discard Calim, conjure a card named Calim,
  -- Djinn Emperor into your library seventh from the top"). Ten Islands under
  -- it, so both the seventh place and the bottom are real places, and the two
  -- readings land the conjured Calim at different indices.
  Spec.it s "a conjure seventh from the top puts the card under six cards" $ do
    calim <- S.printingOf s registry "Calim, Djinn Emperor"
    island <- S.printingOf s registry "Island"
    reference <- mapM (S.cardOf s registry) ["Calim, Djinn Emperor", "Goblin Piker"]
    let (inHand, board) = S.addHandCard calim S.alice (islandsInLibrary island 10 (Setup.emptyGame S.bothPlayers))
        discarded = S.runPure S.identityAnswer board (Event.discard (pure ()) DiscardCause.Ordinary S.alice inHand)
        (final, asked) = conjureDepthRun reference 1 discarded
    Spec.assertEqWith
      s
      "the conjured Calim is seventh from the top of alice's library, above four Islands"
      (libraryIndexOf calimName final, Seq.length (libraryOf final))
      (Just 6, 11)
    Spec.assertEqWith s "a stated depth asks randomness nothing" asked []

  -- Calim's Breath, "{1}{U}, Discard Calim: Tap up to one target nonland
  -- permanent. Draw a card. Then you may exile two other cards named Calim,
  -- Djinn Emperor from your graveyard. When you do, return Calim from your
  -- graveyard to the battlefield tapped." CR 400.7j lets the ability find the
  -- card its own cost discarded (Binding.discardedCard), and "other" is that
  -- card left out of what the gate may exile.
  Spec.it s "CR 400.7j Calim's Breath returns the Calim its cost discarded, tapped" $ do
    (final, _) <- calimsBreath s registry False 2
    Spec.assertEqWith
      s
      "a tapped Calim is on alice's battlefield, and the two others are in exile"
      (fmap (\oid -> Object.tapped <$> Game.lookupObject oid final) (namedIn calimName Zone.Battlefield final), length (namedIn calimName Zone.Exile final))
      ([Just TapState.Tapped], 2)
  -- The pair: ONE other Calim. The discarded Calim is not "other", so the gate
  -- cannot be paid and neither card leaves the graveyard.
  Spec.it s "CR 400.7j Calim's Breath cannot exile the discarded Calim as one of the two others" $ do
    (final, offered) <- calimsBreath s registry False 1
    Spec.assertEqWith
      s
      "both Calims stay in alice's graveyard"
      (length (namedIn calimName Zone.Graveyard final), namedIn calimName Zone.Battlefield final)
      (2, [])
    Spec.assertEqWith s "and alice was never offered the exile" offered 0

  -- Mine Security ({1}{R} Creature -- Kavu Soldier 3/1, trample, "When this
  -- creature enters, conjure a card named Flametongue Kavu into the top eight
  -- cards of your library at random."). Randomness names the fifth place, which
  -- neither end nor the top of the eight is. Its perpetual {0} rider is proven
  -- by the scenario cr-118-9-the-kavu-mine-security-conjured-is-cast-for-0.
  Spec.it s "a conjure into the top eight at random puts the card where randomness named" $ do
    security <- S.printingOf s registry "Mine Security"
    island <- S.printingOf s registry "Island"
    let (_, board) = S.entersWithTrigger security S.alice (islandsInLibrary island 10 (Setup.emptyGame S.bothPlayers))
        (final, asked) = conjureDepthRun [] 5 board
    Spec.assertEqWith
      s
      "the Flametongue Kavu is fifth from the top of alice's library"
      (libraryIndexOf flametongueKavu final, Seq.length (libraryOf final))
      (Just 4, 11)
    Spec.assertEqWith s "randomness was asked once, over the top eight places" asked [8]
  -- The pair above on a two-card library: three places, not eight, and the last
  -- of them is the bottom.
  Spec.it s "a conjure at random into a short library offers only the places it has" $ do
    security <- S.printingOf s registry "Mine Security"
    island <- S.printingOf s registry "Island"
    let (_, board) = S.entersWithTrigger security S.alice (islandsInLibrary island 2 (Setup.emptyGame S.bothPlayers))
        (final, asked) = conjureDepthRun [] 3 board
    Spec.assertEqWith s "randomness was asked over the three places a two-card library has" asked [3]
    Spec.assertEqWith
      s
      "and the third place is under both Islands"
      (libraryIndexOf flametongueKavu final)
      (Just 2)

fearOfChange :: CardName.CardName
fearOfChange = CardName.MkCardName (Text.pack "Fear of Change")

llanowarElves :: CardName.CardName
llanowarElves = CardName.MkCardName (Text.pack "Llanowar Elves")

calimName :: CardName.CardName
calimName = CardName.MkCardName (Text.pack "Calim, Djinn Emperor")

flametongueKavu :: CardName.CardName
flametongueKavu = CardName.MkCardName (Text.pack "Flametongue Kavu")

-- This many Islands in alice's library, each put on the bottom.
islandsInLibrary :: Printing.Printing -> Int -> GameState.GameState -> GameState.GameState
islandsInLibrary island n gs = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) gs [1 .. n]

libraryOf :: GameState.GameState -> Seq.Seq ObjectId.ObjectId
libraryOf gs = Map.findWithDefault Seq.empty S.alice (GameState.library gs)

-- Where a card of this name sits in alice's library, 0 being the top.
libraryIndexOf :: CardName.CardName -> GameState.GameState -> Maybe Int
libraryIndexOf name gs = Seq.findIndexL (\oid -> S.soleFaceName oid gs == name) (libraryOf gs)

-- Calim in alice's hand at her precombat main, two Islands to pay {1}{U}, bob's
-- Goblin Piker as the target, ten Islands in her library for the discard
-- trigger's conjure, `others` more Calims in her graveyard, and, when
-- `leyline`, bob's Leyline of the Void. She activates Calim's Breath and
-- everything resolves; every offer to pay is accepted and counted.
calimsBreath :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> Int -> m (GameState.GameState, Int)
calimsBreath s registry leyline others = do
  calim <- S.printingOf s registry "Calim, Djinn Emperor"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  void_ <- S.printingOf s registry "Leyline of the Void"
  reference <- mapM (S.cardOf s registry) ["Calim, Djinn Emperor"]
  let empty = islandsInLibrary island 10 (S.landsFor island S.alice 2 (Setup.emptyGame S.bothPlayers))
      base = if leyline then snd (S.addPermanent void_ S.bob empty) else empty
      (pikerId, withPiker) = S.addPermanent piker S.bob base
      stocked = List.foldl' (\g _ -> snd (S.addGraveyardCard calim S.alice g)) withPiker [1 .. others]
      (calimId, inHand) = S.addHandCard calim S.alice stocked
      board =
        inHand
          { GameState.turn =
              (GameState.turn inHand)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
      ability = case Game.faceOf calimId board of
        Just face -> take 1 (Face.activatedAbilities face)
        Nothing -> []
      fixture = S.fixtureRegistry reference
      answer :: Asked.Asked GameState.GameState r -> State.State Int r
      answer asked = case Asked.prompt asked of
        Prompt.ChooseToPay {} -> do
          State.modify' (+ 1)
          pure PaymentDecision.Pays
        Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter ((== Just pikerId) . Recipient.objectOf) . snd) sets)
        p -> pure (S.identityAnswer p)
      asking :: Asked.Asked GameState.GameState r -> State.State Int r
      asking = Interpreter.lookingUpCards fixture S.unreachableCorpus answer
      run = do
        (_, activated) <- Engine.runGameAsked asking board (Monad.mapM_ (Activate.activateAbility S.alice calimId) ability)
        Engine.runGameAsked asking activated Engine.priorityLoop
      ((_, final), offered) = State.runState run 0
  pure (final, offered)

-- Settle the pending trigger and resolve it, looking card names up in
-- `reference` and answering every Prompt.RandomDepth with `depth`. The reaches
-- randomness was asked over come back in order.
conjureDepthRun :: [Card.Card] -> Numeric.Natural.Natural -> GameState.GameState -> (GameState.GameState, [Numeric.Natural.Natural])
conjureDepthRun reference depth board =
  let fixture = S.fixtureRegistry reference
      answer :: Asked.Asked GameState.GameState r -> State.State [Numeric.Natural.Natural] r
      answer asked = case Asked.prompt asked of
        Prompt.RandomDepth reach -> do
          State.modify' (reach :)
          pure depth
        p -> pure (S.identityAnswer p)
      run = do
        (_, settled) <- Engine.runGameAsked (Interpreter.lookingUpCards fixture S.unreachableCorpus answer) board Engine.settleForPriority
        Engine.runGameAsked (Interpreter.lookingUpCards fixture S.unreachableCorpus answer) settled Engine.priorityLoop
      ((_, final), logged) = State.runState run []
   in (final, reverse logged)

giantSpider :: CardName.CardName
giantSpider = CardName.MkCardName (Text.pack "Giant Spider")

ancientVendetta :: CardName.CardName
ancientVendetta = CardName.MkCardName (Text.pack "Ancient Vendetta")

-- Fear of Change enters under alice, beside a Goblin Piker of hers where
-- `withPiker` says so, and its trigger resolves through
-- Parun.Interpreter.lookingUpCards over a FIXTURE reference: Goblin Piker, Hill
-- Giant, Ancient Vendetta and Giant Spider, in that order. Where `lying` says
-- so, Prompt.ReferenceCards is answered with all four instead. Randomness is
-- answered with the first of Goblin Piker, Ancient Vendetta and Giant Spider
-- offered, FILTERED out of the offer, and the head otherwise; every offer is
-- logged.
fearOfChangeRun :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> Bool -> m (GameState.GameState, [[CardName.CardName]])
fearOfChangeRun s registry withPiker lying = do
  fear <- S.printingOf s registry "Fear of Change"
  piker <- S.printingOf s registry "Goblin Piker"
  reference <- mapM (S.cardOf s registry) ["Goblin Piker", "Hill Giant", "Ancient Vendetta", "Giant Spider"]
  let board0 = if withPiker then snd (S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)) else Setup.emptyGame S.bothPlayers
      (_, entered) = S.entersWithTrigger fear S.alice board0
      fixture = S.fixtureRegistry reference
      answer :: Asked.Asked GameState.GameState r -> State.State [[CardName.CardName]] r
      answer asked = case Asked.prompt asked of
        Prompt.RandomCard offered -> do
          State.modify' (NonEmpty.toList offered :)
          pure (Maybe.fromMaybe (NonEmpty.head offered) (List.find (`elem` NonEmpty.toList offered) [goblinPiker, ancientVendetta, giantSpider]))
        p -> pure (S.identityAnswer p)
      asking :: Asked.Asked GameState.GameState r -> State.State [[CardName.CardName]] r
      asking asked = case Asked.prompt asked of
        Prompt.ReferenceCards {}
          | lying -> pure (fmap S.nameOf reference)
        _ -> Interpreter.lookingUpCards fixture S.unreachableCorpus answer asked
      run = do
        (_, settled) <- Engine.runGameAsked asking entered Engine.settleForPriority
        Engine.runGameAsked asking settled Engine.priorityLoop
      ((_, final), logged) = State.runState run []
  pure (final, reverse logged)

goblinPiker :: CardName.CardName
goblinPiker = CardName.MkCardName (Text.pack "Goblin Piker")

hillGiant :: CardName.CardName
hillGiant = CardName.MkCardName (Text.pack "Hill Giant")

-- CR 601.2c's announcement and choice in one, pinned to the named objects:
-- announce as many as there are and hand back exactly those. Pinned rather than
-- searched, Parun.CopySpec's posture, so a mutation cannot be repaired by the
-- answerer finding some other legal target.
aimingAtAll :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
aimingAtAll oids p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (Natural.length oids)) offers
  -- FILTERED out of the offered recipients rather than built: CR 608.2b
  -- re-reads the targets at resolution, and a recipient assembled here would
  -- carry the wrong tag for the slot's pool.
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, rs) -> Set.filter (maybe False (`elem` oids) . Recipient.objectOf) rs) asked
  _ -> S.identityAnswer p

-- A Clone resolved as a copy of `original`, then duplicated into alice's hand
-- by Sinister Reflections: the duplicate and the board, the original and the
-- Clone still on the battlefield.
conjuredDuplicate :: Printing.Printing -> Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> Maybe (ObjectId.ObjectId, GameState.GameState)
conjuredDuplicate clone reflections original board0 = do
  let (_, staged) = S.spellOnStack clone S.alice board0
      entered = S.settleSba (copyingPiker original staged)
  cloneId <- Maybe.listToMaybe (clonesOnBattlefield entered)
  let (spell, board1) = S.addHandCard reflections S.alice entered
      board =
        board1
          { GameState.turn =
              (GameState.turn board1)
                { TurnRegisters.phase = Phase.PrecombatMain
                }
          }
      resolved = S.settleSba (S.runPure (aimingAtAll [cloneId]) board (S.cast S.alice spell >> Stack.resolveTop))
  duplicate <- Maybe.listToMaybe (namedIn cloneName Zone.Hand resolved)
  pure (duplicate, resolved)

-- Resolves the staged Clone with its CR 614.12 as-enters copy pinned to the
-- named permanent, Parun.CopySpec's copyNamed for its reason: a searching
-- answerer could repair a mutation by finding some other legal source.
copyingPiker :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
copyingPiker oid gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseCopyTarget {} -> Just oid
        _ -> S.identityAnswer p
   in S.runPure answer gs Stack.resolveTop

-- The battlefield objects whose PRINTED card is Clone, which is what tells the
-- copy from the Piker it copied -- every projected characteristic of the two is
-- the same.
clonesOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
clonesOnBattlefield gs = filter (\oid -> S.soleFaceName oid gs == cloneName) (Game.zoneMembers Zone.Battlefield S.alice gs)

-- Is casting this card as the named half among alice's legal actions, with
-- priority hers? The offer, which Cast.castable asked of a name does not check.
castOffered :: ObjectId.ObjectId -> CardName.CardName -> GameState.GameState -> Bool
castOffered oid name gs =
  elem
    (Action.Cast oid name Facing.FaceUp)
    ( Action.legalActions
        S.alice
        gs
          { GameState.turn =
              (GameState.turn gs)
                { TurnRegisters.priority = Just S.alice
                }
          }
    )

cloneName :: CardName.CardName
cloneName = CardName.MkCardName (Text.pack "Clone")

-- The object a mana activation names: the tapped case reads WHICH permanents
-- alice may tap.
manaSource :: Action.Action -> Maybe ObjectId.ObjectId
manaSource action = case action of
  Action.ActivateManaAbility oid -> Just oid
  _ -> Nothing
