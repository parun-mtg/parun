{-# LANGUAGE GADTs #-}

-- Covers: CR 406.3's face-down exile -- Object.exiledFaceDown, CR 406.3a's
-- absent characteristics as Parun.Engine.Projection.View.baseCharacteristics
-- seeds them and as Parun.Engine.Projection.gatherGiven's exile walk reads them,
-- CR 406.3a's turn face up as the card is played
-- (Parun.Engine.Cast.turnedUpForPlay), the EntryRiders.exiledFaceDown rider
-- Parun.Engine.Event.changeZoneEntering reads,
-- CR 406.3's instruction-given look permission -- Object.exileLookers as
-- Effect.GrantLookAtExiled writes it, whether a spell's own effect writes it or
-- CR 702.75a's hideaway does, and as Parun.Engine.Exile.accrueLookers samples rule
-- 406.3's continuing permission onto it --
-- CR 406.4's two halves over Parun.Engine.Target's Pool.CardsInExile arm -- the
-- permission Parun.Engine.Exile.mayLookAt answers, the pile
-- Parun.Engine.Exile.pileOf sorts a card into and Parun.Engine.Target's piledOffer
-- offers instead, and the draw drawFromPiles takes out of it, taken over the
-- WHOLE pile, asked with Game.ask rather than Game.choose (CR 104.4b), and
-- revealed only once a cast or activation is paid for -- and
-- ObjectRef.EachCardInYourHand as Parun.Engine.Resolve sweeps it.
--
-- The readings a scenario can play and check live in data/scenarios/exile: CR
-- 406.4's separate piles, its draw elided at a one-card pile and filtered back
-- against the pile, the foretold card's turn face up, Extract Power's looker,
-- Runic Repetition's flashback slot, and Windbrisk Heights' hideaway, look and
-- play ability.
--
-- Gameplay-level, off three producers that exile face down and differ in exactly
-- the permission. Ignorant Bliss {1}{R} Instant -- "Exile all cards from your
-- hand face down" -- grants nobody a look, so not even the owner may choose what
-- it exiled; foretell (CR 702.143a) grants the owner one, so she may; Extract
-- Power looks at the cards before hiding them, which rule 406.3 turns into a
-- permission for the LOOKER, who owns only one of them. Synthetic
-- Blind Reclamation's unqualified "target exiled card" is what reads the
-- difference back out, and Riftsweeper's printed "face-up exiled card" is the
-- third reading -- a card whose own words refuse what rule 406.4 offers.
-- Synthetic Ledger Glimpse's "target card you own in exile" is the owner-scoped
-- slot a draw out of Extract Power's two-owner pile can land outside of.
--
-- Each group shares ONE board across its readings, which is the point: exile
-- holds the same cards either way, and only how they got there differs.
module Parun.ExileSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Parun.Engine.Activatable as Activatable
import qualified Parun.Engine.Activate as Activate
import qualified Parun.Engine.Departure as Departure
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Exile as Exile
import qualified Parun.Engine.Foretell as Foretell
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Modal as Modal
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Engine.Setup as Setup
import qualified Parun.Engine.Stack as Stack
import qualified Parun.Engine.Target as Target
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.Action as A
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.BeginningStep as BeginningStep
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Departure as Departure.Type
import qualified Parun.Types.EndingStep as EndingStep
import qualified Parun.Types.Face as Face
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.OptionalDecision as OptionalDecision
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.Pile as Pile
import qualified Parun.Types.PileDraw as PileDraw
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView.Type
import qualified Parun.Types.Printing as Printing
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Recipient as Recipient
import qualified Parun.Types.Revealed as Revealed
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.StackObjectKind as StackObjectKind
import qualified Parun.Types.StepBegan as StepBegan
import qualified Parun.Types.StoredEffects as StoredEffects
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TriggeredAbility as TriggeredAbility
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Face-down exile" $ do
  foretold s registry
  extractPower s registry
  glimpseRefusedDraws s registry
  windbriskHeights s registry
  Spec.describe s "Ignorant Bliss" $ do
    -- CR 406.3a and CR 406.4's first half, read through the pool that offers
    -- exiled cards as targets. The board is deliberately one board: alice's two
    -- hand cards go to exile face down by casting the card, and a THIRD card is
    -- already sitting there face up. A gate that had not been written would offer
    -- all three; a gate that emptied the pool outright would offer none.
    --
    -- Read through the UNQUALIFIED slot, so what refuses the two face-down cards
    -- is rule 406.4 rather than a card's printed word -- and refuses them to
    -- ALICE, who owns them and cast the spell that hid them. Ignorant Bliss
    -- grants no look, and CR 406.3's permission comes only from an instruction
    -- that gives one; owning the card is not one.
    Spec.it s "CR 406.4 a card exiled face down is named by nobody, not even its owner, and offered as the pile it is in" $ do
      board <- castBliss s registry
      reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
      case S.spellTargetSlot reclamation of
        Just theSlot -> do
          Spec.assertEqWith s "all three cards are in the exile zone" (Set.size (GameState.exile board)) 3
          Spec.assertEqWith
            s
            "the offer names the face-up card and the PILE the two face-down ones are in (CR 406.4)"
            (offerTo S.alice theSlot board)
            (Set.union (Set.fromList (fmap firstDraw (pilesIn board))) (Set.fromList (fmap Recipient.ToObject (faceUpExiled board))))
          Spec.assertEqWith s "and that is ONE pile, both cards having been hidden by one casting" (length (pilesIn board)) 1
          Spec.assertEqWith s "and the face-up card is exactly one" (length (faceUpExiled board)) 1
          -- The other half of the same rule: the pile stands for cards that are
          -- LEGAL targets -- CR 406.4 restricts the announcement, not legality,
          -- and CR 608.2b re-derives this set at resolution.
          Spec.assertEqWith
            s
            "while all three remain legal targets, which is what the drawn card needs at CR 608.2b"
            (Target.legalRecipients StackObjectKind.Spell (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject (Set.toList (GameState.exile board))))
        Nothing -> Spec.assertFailure s "Synthetic Blind Reclamation should print one target slot"
    -- CR 406.4's last sentence, read through each seat's PlayerView, the view a
    -- client is handed: the card the draw named is not revealed until the cost
    -- is paid, and is then revealed to both seats (docs/synthetic-readings.md,
    -- "Whether a target drawn out of a face-down pile is revealed"). The
    -- prompts between the draw and the end of the cast are CR 601.2g's mana
    -- window, which ChooseManaSource raises and never elides.
    --
    -- The other card of the pile is the control: revealed to nobody before or
    -- after, so a reveal of the whole pile cannot pass.
    Spec.it s "CR 406.4 the card drawn for a spell's target is hidden while it is paid for, and revealed to both seats once cast" $ do
      (drawn, other, spellId, before) <- pileBoardUncast s registry
      let ((_, after), seen) = State.runState (Engine.runGameAsked watchingPayment before (S.cast S.alice spellId)) Nothing
      revealedOncePaid s drawn other before after seen
    -- The same through CR 602.2b, which runs an activation through CR
    -- 601.2b-i: Synthetic Blind Reclaimer's "{1}, {T}: Shuffle target exiled
    -- card into its owner's library" in place of the instant.
    Spec.it s "CR 406.4 / 602.2b the card drawn for an ability's target is hidden while it is paid for, and revealed to both seats once activated" $ do
      (drawn, other, _, uncast) <- pileBoardUncast s registry
      reclaimer <- S.printingOf s registry "Synthetic Blind Reclaimer"
      let (reclaimerId, before) = S.addPermanent reclaimer S.alice uncast
      case Activatable.abilitiesFor reclaimerId before of
        [ability] -> do
          let ((_, after), seen) = State.runState (Engine.runGameAsked watchingPayment before (Activate.activateAbility S.alice reclaimerId ability)) Nothing
          revealedOncePaid s drawn other before after seen
        _ -> Spec.assertFailure s "Synthetic Blind Reclaimer should state one activated ability"
    -- CR 104.4b, Parun.InvestigateSpec's random reveal one rule over: being asked
    -- for randomness is not being offered a CHOICE, so CR 406.4's draw goes
    -- through Game.ask and leaves Bookkeeping.lastChoice alone. Otherwise a loop
    -- of mandatory actions containing such a draw would look interruptible and
    -- Parun.Engine.Departure.checkMandatoryLoop could never call it a draw.
    --
    -- Driven through drawFromPiles itself rather than through a cast, because
    -- announcing the target is a choice (Prompt.ChooseTargets) and would stamp
    -- the field either way.
    Spec.it s "CR 104.4b the draw out of a pile is not an optional action" $ do
      exiled <- castBliss s registry
      case pilesIn exiled of
        [pile] -> do
          let board = exiled {GameState.bookkeeping = (GameState.bookkeeping exiled) {Bookkeeping.lastChoice = Timestamp.MkTimestamp 0}}
              (drawn, after) = S.runPureWith throughPile board (fst <$> Target.drawFromPiles (Just S.alice) (Set.singleton (firstDraw pile)))
          case membersOfPile board pile of
            [_, second] -> Spec.assertEqWith s "the draw was honoured, naming the last card of the pile" (Set.toList drawn) [Recipient.ToObject second]
            _ -> Spec.assertFailure s "the casting should hide two cards in one pile"
          Spec.assertEqWith s "and nobody was recorded as having been offered a choice" (Bookkeeping.lastChoice (GameState.bookkeeping after)) (Timestamp.MkTimestamp 0)
        _ -> Spec.assertFailure s "one casting should make one pile"
    -- CR 406.4 under CR 115.3: a slot wanting TWO exiled cards may name the one
    -- pile twice, each naming a draw, and the second draw is over what the first
    -- left. The face-up Ogre Sentry is offered too, so a slot offered the pile
    -- only once could still be filled -- just not out of the pile alone, which is
    -- what the answerer asks for.
    Spec.it s "CR 406.4 a slot wanting two cards names one pile twice, and both of its cards are drawn" $ do
      exiled <- castBliss s registry
      plains <- S.printingOf s registry "Plains"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      sentry <- S.printingOf s registry "Ogre Sentry"
      recall <- S.printingOf s registry "Synthetic Twin Recall"
      let (spellId, g1) = S.addHandCard recall S.alice (S.landsFor plains S.alice 2 exiled)
          board =
            g1
              { GameState.turn =
                  (GameState.turn g1)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice
                    }
              }
          after = resolveAll (S.runPure throughPile board (S.cast S.alice spellId))
      Spec.assertBool
        s
        (Set.isSubsetOf (Set.fromList [S.printingName piker, S.printingName bolt]) (namesIn Zone.Graveyard S.alice after))
        "both cards of the one pile reached alice's graveyard"
      -- Proxies, AFTER the behaviour so neither can absorb a mutation.
      Spec.assertEqWith s "the face-up card stayed in exile" (namesOf (Set.toList (GameState.exile after)) after) (Set.singleton (S.printingName sentry))
      case S.spellTargetSlot recall of
        Just theSlot -> Spec.assertEqWith s "the pile was offered twice beside the face-up card" (Set.size (offerTo S.alice theSlot board)) 3
        Nothing -> Spec.assertFailure s "Synthetic Twin Recall should print one target slot"
    -- The same over CR 707.10c's road (Parun.Engine.Resolve.Effect's
    -- chooseNewTargetsFor): the original aims at two face-up cards, and its
    -- Twincast copy re-chooses both of its targets out of the one pile.
    Spec.it s "CR 707.10c a copy wanting two cards names one pile twice, and both of its cards are drawn" $ do
      exiled <- castBliss s registry
      plains <- S.printingOf s registry "Plains"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      riftsweeper <- S.printingOf s registry "Riftsweeper"
      recall <- S.printingOf s registry "Synthetic Twin Recall"
      twincast <- S.printingOf s registry "Twincast"
      let (_, g1) = S.addExiledCard riftsweeper S.bob (S.landsFor island S.alice 2 (S.landsFor plains S.alice 2 exiled))
          (recallId, g2) = S.addHandCard recall S.alice g1
          (twincastId, g3) = S.addHandCard twincast S.alice g2
          board =
            g3
              { GameState.turn =
                  (GameState.turn g3)
                    { TurnRegisters.activePlayer = S.alice,
                      TurnRegisters.phase = Phase.PrecombatMain,
                      TurnRegisters.priority = Just S.alice
                    }
              }
          isPile recipient = case recipient of
            Recipient.ToPile _ -> True
            _ -> False
          faceUpOnly :: Prompt.Prompt r -> r
          faceUpOnly p = case p of
            Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (not . isPile) . snd) sets
            _ -> S.identityAnswer p
          aimed = S.runPure S.identityAnswer (S.runPure faceUpOnly board (S.cast S.alice recallId)) (S.cast S.alice twincastId)
          after = S.runPure throughPile aimed Engine.priorityLoop
      Spec.assertBool
        s
        (Set.isSubsetOf (Set.fromList [S.printingName piker, S.printingName bolt]) (namesIn Zone.Graveyard S.alice after))
        "both cards of the one pile reached alice's graveyard through the copy"
      -- Proxy, AFTER the behaviour: the original still resolved on its two
      -- face-up targets, so exile is empty.
      Spec.assertEqWith s "and the original took the two face-up cards" (Set.size (GameState.exile after)) 0
    -- The same gate from the other side, on a board differing in ONE thing: the
    -- two cards reach exile FACE UP instead, by the same route every other test
    -- puts a card there. Same printings, same seats, same zone, same count -- so a
    -- pool that had simply lost its exile candidates could not pass this.
    Spec.it s "CR 406.3 the same two cards exiled FACE UP are all legal targets" $ do
      riftsweeper <- S.printingOf s registry "Riftsweeper"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      sentry <- S.printingOf s registry "Ogre Sentry"
      let (upA, g1) = S.addExiledCard piker S.alice (Setup.emptyGame S.bothPlayers)
          (upB, g2) = S.addExiledCard bolt S.alice g1
          (upC, board) = S.addExiledCard sentry S.bob g2
      case triggerTargetSlot riftsweeper of
        Just theSlot ->
          Spec.assertEqWith
            s
            "CR 406.3's face-up default leaves every exiled card choosable"
            (Target.legalRecipients StackObjectKind.Spell (Just S.alice) S.noSource theSlot board)
            (Set.fromList (fmap Recipient.ToObject [upA, upB, upC]))
        Nothing -> Spec.assertFailure s "Riftsweeper should print one triggered ability with one target slot"
    -- CR 400.7 through the delayed ability: "return THOSE CARDS to your hand, then
    -- draw a card". The slot the exiling move bound is what survives the
    -- resolution (CR 603.7c), and the cards come back as ordinary face-up cards in
    -- a hand -- which is Object.exiledFaceDown being per-incarnation state.
    Spec.it s "CR 603.7 at the next end step the exiled cards return to hand and alice draws" $ do
      board <- castBliss s registry
      let after = endStep board
      Spec.assertEqWith s "alice's hand was emptied by the spell" (S.handSize S.alice board) 0
      Spec.assertEqWith s "two cards returned, plus the draw" (S.handSize S.alice after) 3
      Spec.assertEqWith s "only the face-up card is left in exile" (Set.size (GameState.exile after)) 1
      -- A REGRESSION FENCE, not a proof: the returning move carries the default
      -- rider, so its arrival is face up by construction rather than by any
      -- guard. Dropping Event.changeZoneAttaching's destination gate leaves
      -- this green.
      Spec.assertEqWith s "and nothing in a hand is face down in exile" (concealedIn Zone.Hand S.alice after) []
      Spec.assertEqWith s "the delayed ability was spent" (length (StoredEffects.delayedTriggers (GameState.stored after))) 0
    -- CR 406.3a: a card exiled face down has NO characteristics, so a CR 113.6c
    -- ability that functions from exile does not function from under one.
    -- Grist, the Hunger Tide is a 1/1 Insect creature card in every zone but the
    -- battlefield, which Parun.ProjectionSpec's HiddenZoneStatics group reads off
    -- a face-up exiled Grist; here the same card reaches the same zone by
    -- Ignorant Bliss instead, and the pair differs in exactly one thing -- CR
    -- 406.3's face-down flag.
    Spec.it s "CR 406.3a a Grist card exiled FACE DOWN has no characteristics to function from" $ do
      bliss <- S.printingOf s registry "Ignorant Bliss"
      mountain <- S.printingOf s registry "Mountain"
      grist <- S.printingOf s registry "Grist, the Hunger Tide"
      piker <- S.printingOf s registry "Goblin Piker"
      let (g1, blissId) = S.handOne bliss (S.landsInPlay mountain 2)
          (_, g2) = S.addHandCard grist S.alice g1
          (_, g3) = S.addLibraryCard piker S.alice g2
          hidden = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 (S.cast S.alice blissId)) Engine.priorityLoop
          (faceUpGrist, shown) = S.addExiledCard grist S.alice (Setup.emptyGame S.bothPlayers)
      case faceDownExiled hidden of
        [downGrist] -> do
          Spec.assertEqWith
            s
            "no 1/1 under the face-down flag, where the same card exiled face up is one"
            (S.powerToughnessOf downGrist hidden, S.powerToughnessOf faceUpGrist shown)
            (Nothing, Just (1, 1))
          Spec.assertEqWith s "and it is the Grist that Ignorant Bliss hid" (namesOf [downGrist] hidden) (Set.singleton (S.printingName grist))
        _ -> Spec.assertFailure s "the casting should hide exactly the Grist"

-- CR 702.143a's foretold card carries CR 406.3's permission to look at a card
-- exiled face down off its stamp rather than off an opcode, and CR 406.4 is what
-- turns a permission to
-- LOOK into a permission to CHOOSE: "the player may choose a specific face-down
-- card only if the player is allowed to look at that card".
--
-- ONE board: alice foretells Augury Raven and bob has an Ogre Sentry exiled face
-- up, so exile holds two cards under either reading and a pool that had simply
-- lost its exile candidates could not pass.
foretold :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
foretold s registry = Spec.describe s "Augury Raven" $ do
  -- CR 702.143f: a departing player reveals every face-down foretold card they
  -- own. alice foretells Augury Raven in a three-player game and concedes; the
  -- negative leg concedes with the Raven still in her hand.
  Spec.it s "CR 702.143f a face-down foretold card is revealed when its owner leaves the game" $ do
    raven <- S.printingOf s registry "Augury Raven"
    island <- S.printingOf s registry "Island"
    let (handRaven, g1) = S.addHandCard raven S.alice (S.landsFor island S.alice 2 S.threePlayerGame)
        before =
          g1
            { GameState.turn =
                (GameState.turn g1)
                  { TurnRegisters.activePlayer = S.alice,
                    TurnRegisters.phase = Phase.PrecombatMain,
                    TurnRegisters.priority = Just S.alice
                  }
            }
        foretoldGs = S.runPure S.identityAnswer before (Foretell.foretell S.manaPerformer S.alice handRaven)
        concede board = S.runPure S.identityAnswer board (Departure.leaveGame Departure.Type.Conceded S.alice)
        revealsIn board = [(Revealed.player r, Revealed.card r) | GameEvent.Revealed r <- S.eventsOf board]
    case faceDownExiled foretoldGs of
      [ravenId] -> do
        Spec.assertEqWith s "CR 702.143f alice revealed her foretold card" (revealsIn (concede foretoldGs)) [(S.alice, ravenId)]
        Spec.assertEqWith s "a card in her hand leaving with her reveals nothing" (revealsIn (concede before)) []
      _ -> Spec.assertFailure s "setup: the Raven was not foretold"
  -- The permission read off the pool, and the third leg is what keeps CR
  -- 406.4's permission and Riftsweeper's printed qualifier from being one
  -- question: rule 406.4 OFFERS alice her foretold card, and Riftsweeper's own
  -- "face-up exiled card" then refuses it.
  Spec.it s "CR 406.4 the pool names a face-down exiled card to a player who may look at it, and offers the others its pile" $ do
    reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
    riftsweeper <- S.printingOf s registry "Riftsweeper"
    (downId, upId, _, _, board) <- foretoldBoard s registry
    case (S.spellTargetSlot reclamation, triggerTargetSlot riftsweeper) of
      (Just unqualified, Just faceUpOnly) -> do
        Spec.assertEqWith
          s
          "alice is offered both exiled cards by name"
          (offerTo S.alice unqualified board)
          (Set.fromList (fmap Recipient.ToObject [downId, upId]))
        -- CR 702.143e keeps a foretold card differentiable from every other
        -- face-down card its owner owns, so its pile holds it alone -- and the
        -- pile bob is offered is that one rather than alice's face-down cards at
        -- large.
        Spec.assertEqWith
          s
          "bob is offered the face-up one by name and the foretold card only as its own pile"
          (offerTo S.bob unqualified board)
          (Set.fromList [Recipient.ToObject upId, firstDraw (Pile.OfForetold (timestampOf downId board))])
        Spec.assertEqWith
          s
          "and Riftsweeper's printed face-up qualifier refuses the foretold card even to alice, offering no pile in its place"
          (offerTo S.alice faceUpOnly board)
          (Set.singleton (Recipient.ToObject upId))
      _ -> Spec.assertFailure s "Riftsweeper and Synthetic Blind Reclamation should each print one target slot"

-- alice holds four Islands and foretells Augury Raven off two of them (CR
-- 116.2h), leaving exactly the {1}{U} her copy of the instant costs; bob holds
-- two Islands and a copy of his own. An Ogre Sentry sits in exile face up under
-- bob, so the pool is never empty.
--
-- Returns the face-down foretold card, the face-up one, and each player's spell.
foretoldBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
foretoldBoard s registry = do
  island <- S.printingOf s registry "Island"
  raven <- S.printingOf s registry "Augury Raven"
  sentry <- S.printingOf s registry "Ogre Sentry"
  reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
  let (ravenId, g1) = S.addHandCard raven S.alice (S.landsInPlay island 4)
      (upId, g2) = S.addExiledCard sentry S.bob g1
      (aliceSpell, g3) = S.addHandCard reclamation S.alice g2
      (bobSpell, g4) = S.addHandCard reclamation S.bob g3
      g5 = S.landsFor island S.bob 2 g4
      before =
        g5
          { GameState.turn =
              (GameState.turn g5)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
      board = S.runPure S.identityAnswer before (Foretell.foretell S.manaPerformer S.alice ravenId)
      downId = case faceDownExiled board of
        [only] -> only
        _ -> S.noSource
  pure (downId, upId, aliceSpell, bobSpell, board)

-- CR 406.3's OTHER permission, the one an instruction gives: "if a player is
-- instructed to look at a card and then exile it face down ... that player may
-- continue to look at that card until it leaves the exile zone". Extract Power
-- {5}{U} Sorcery -- "Look at the top card of each player's library, then exile
-- those cards face down. You may play them without paying their mana costs for
-- as long as they remain exiled" (Oracle text checked 2026-09-08) -- is the
-- producer, and it is the one shape that tells the permission apart from
-- OWNERSHIP: alice is instructed to look at bob's card too, so she may name a
-- card she does not own, while bob may name neither -- not even his own.
--
-- Against Ignorant Bliss above, which is the same board minus the instruction:
-- that spell exiles cards face down without looking at them first, and its
-- owner may name none of them.
--
-- The card is transcribed whole: its GrantPlayFromExile carries CR 118.9's
-- waiver, which the case below proves by casting one of the exiled cards off a
-- board with no untapped land on it.
--
-- CR 406.3a's turn face up rides Parun.Engine.Cast.turnedUpForPlay, so both
-- cards are offered and cast with their characteristics back; the foretold
-- card's Thalia scenario in data/scenarios/exile is what proves the turn. CR
-- 110.5d keeps that apart from Object.facing, which `proposedFace` branches on
-- and no exile rider writes.
extractPower :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
extractPower s registry = Spec.describe s "Extract Power" $ do
  -- CR 118.9's other clause of the same sentence: "You may play them WITHOUT
  -- PAYING THEIR MANA COSTS for as long as they remain exiled". The permission
  -- Parun.Engine.Cost prices the cast from is the only thing making it legal, so
  -- the waiver replaces the printed cost rather than joining it.
  --
  -- The board is the group's own, which is what makes the claim about the COST
  -- rather than about mana: Extract Power {5}{U} took every one of alice's six
  -- Islands, so she has nothing at all to spend, and the Goblin Piker she
  -- exiled off her own library prints {1}{R} -- a cost those Islands could not
  -- have paid even untapped. Casting it is therefore possible only because
  -- nothing is owed.
  Spec.it s "CR 118.9 the exiled card is cast for nothing, off a board with no untapped land and no red source" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    board <- castExtractPower s registry
    case filter (\oid -> fmap S.nameOf (Game.cardOf oid board) == Just (S.printingName piker)) (faceDownExiled board) of
      [pikerId] -> do
        let resolved = S.runPure S.castAnswer board (S.cast S.alice pikerId >> Stack.resolveTop)
        -- The GATE and the CAST are two roads through Cost.candidateCostsGiven
        -- and both are asserted, so a waiver honoured by one and not the other
        -- cannot pass. Each was mutated red on its own: the gate here, and the
        -- battlefield below with this line ordered after it.
        Spec.assertBool s (S.castable S.alice pikerId board) "the cast is offered: CR 601.3's permission with a cost alice can pay"
        Spec.assertEqWith
          s
          "and the Goblin Piker is on the battlefield, so the announcement found that cost too"
          (S.countOnBattlefieldByName (S.printingName piker) S.alice resolved)
          1
        -- A proxy, AFTER both behavioural assertions so neither can absorb a
        -- mutation: the six Islands that paid for Extract Power are still the
        -- only tapped permanents alice has.
        Spec.assertEqWith s "alice's six Islands are still the only tapped permanents she has" (S.tappedCount S.alice resolved) 6
      _ -> Spec.assertFailure s "Extract Power should exile alice's Goblin Piker face down"

-- CR 406.4's draw landing on a card the slot REFUSES, on the two roads that
-- re-aim a spell already on the stack. Extract Power's pile holds one card of
-- each owner and bob may look at neither, so Synthetic Ledger Glimpse's "target
-- card you own in exile" is offered to him as the pile, and a draw pinned to
-- alice's Goblin Piker names a card it refuses. The new target must be legal
-- (CR 707.10c, CR 115.7a), so the spell keeps its old target, a face-up card of
-- bob's, and resolves: the Glimpse's draw is what tells that from a spell
-- recording the illegal card and being countered (CR 608.2b).
glimpseRefusedDraws :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
glimpseRefusedDraws s registry = Spec.describe s "Ledger Glimpse" $ do
  Spec.it s "CR 707.10c a copy whose re-choice draws a card it refuses keeps its old target, so both spells resolve" $ do
    (board, faceUp, pikerId, glimpseId, twincastId, _) <- glimpseBoard s registry
    let aimed = S.runPure S.identityAnswer (S.runPure (aimedAt faceUp) board (S.cast S.bob glimpseId)) (S.cast S.bob twincastId)
        after = S.runPure (drawing pikerId) aimed Engine.priorityLoop
    Spec.assertEqWith s "the copy and the original each drew bob a card" (libraryDrop board after) 2
    -- Proxy, AFTER the behaviour: the re-choice was offered the pile.
    glimpseOffersPile s registry board faceUp
  Spec.it s "CR 115.7a a change of target that draws a card the spell refuses leaves the old target, so the spell resolves" $ do
    (board, faceUp, pikerId, glimpseId, _, deflectionId) <- glimpseBoard s registry
    let aimed = S.runPure S.identityAnswer (S.runPure (aimedAt faceUp) board (S.cast S.bob glimpseId)) (S.cast S.bob deflectionId)
        after = S.runPure (drawing pikerId) aimed Engine.priorityLoop
    Spec.assertEqWith s "the Glimpse still resolved on its face-up target and drew bob a card" (libraryDrop board after) 1
    glimpseOffersPile s registry board faceUp

-- How many cards left bob's library between two boards.
libraryDrop :: GameState.GameState -> GameState.GameState -> Int
libraryDrop before after = length (Game.zoneMembers Zone.Library S.bob before) - length (Game.zoneMembers Zone.Library S.bob after)

-- Anti-vacuity for the Ledger Glimpse pair: bob is offered his face-up card by
-- name and the two-owner pile in place of his own hidden card.
glimpseOffersPile :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> GameState.GameState -> ObjectId.ObjectId -> m ()
glimpseOffersPile s registry board faceUp = do
  glimpse <- S.printingOf s registry "Synthetic Ledger Glimpse"
  case S.spellTargetSlot glimpse of
    Just theSlot ->
      Spec.assertEqWith
        s
        "bob is offered his face-up card and the pile (CR 406.4)"
        (offerTo S.bob theSlot board)
        (Set.insert (Recipient.ToObject faceUp) (Set.fromList (fmap firstDraw (pilesIn board))))
    Nothing -> Spec.assertFailure s "Synthetic Ledger Glimpse should print one target slot"

-- castExtractPower's board, with bob holding priority, five Islands, three more
-- library cards for the draws, a face-up Riftsweeper of his in exile, and
-- Synthetic Ledger Glimpse, Twincast and Deflection in hand. Returns the board,
-- the Riftsweeper, alice's hidden Goblin Piker and the three spells.
glimpseBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
glimpseBoard s registry = do
  exiled <- castExtractPower s registry
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  riftsweeper <- S.printingOf s registry "Riftsweeper"
  glimpse <- S.printingOf s registry "Synthetic Ledger Glimpse"
  twincast <- S.printingOf s registry "Twincast"
  deflection <- S.printingOf s registry "Deflection"
  let stocked = foldr (\_ g -> snd (S.addLibraryCard sentry S.bob g)) (S.landsFor island S.bob 5 exiled) [1 .. (3 :: Int)]
      (faceUp, g1) = S.addExiledCard riftsweeper S.bob stocked
      (glimpseId, g2) = S.addHandCard glimpse S.bob g1
      (twincastId, g3) = S.addHandCard twincast S.bob g2
      (deflectionId, g4) = S.addHandCard deflection S.bob g3
      board =
        g4
          { GameState.turn =
              (GameState.turn g4)
                { TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.bob
                }
          }
      pikerId = Maybe.fromMaybe S.noSource (List.find (\oid -> namesOf [oid] board == Set.singleton (S.printingName piker)) (faceDownExiled board))
  pure (board, faceUp, pikerId, glimpseId, twincastId, deflectionId)

-- alice casts Extract Power off six Islands. Each library's top card is a
-- DIFFERENT one, so a failure names which seat's card came out where, and each
-- holds a second card beneath it so neither library empties (CR 104.3c).
castExtractPower :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castExtractPower s registry = do
  power <- S.printingOf s registry "Extract Power"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let (g1, powerId) = S.handOne power (S.landsInPlay island 6)
      (_, g2) = S.addLibraryCard bolt S.alice g1
      (_, g3) = S.addLibraryCard piker S.alice g2
      (_, g4) = S.addLibraryCard bolt S.bob g3
      (_, g5) = S.addLibraryCard sentry S.bob g4
  pure (S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice powerId)) Engine.priorityLoop)

-- CR 702.75a's hideaway N, an instruction-given look that no spell gives:
-- "When this permanent enters, look at the top N cards of your library. Exile one
-- of them face down and put the rest on the bottom of your library in a random
-- order. The exiled card gains 'The player who controls the permanent that
-- exiled this card may look at this card in the exile zone.'" Windbrisk Heights
-- Land -- "Hideaway 4 ... This land enters tapped. {T}: Add {W}. {W}, {T}: You
-- may play the exiled card without paying its mana cost if you attacked with
-- three or more creatures this turn" (Oracle text checked 2026-09-08) -- is the
-- producer.
--
-- The ability printed under it is Effect.OfferCast over
-- ObjectRef.EachCardInExiledPile -- CR 607.2a's linked set -- under a
-- Clause.condition comparing Quantity.AttackersDeclaredThisTurn against 3,
-- under PermissionVerb.Play so a hidden land is offered as CR 305.2a's land play.
--
-- The hideaway's look and the ability's link and threshold are scenarios in
-- data/scenarios/exile; this group keeps Null Chamber's prohibition.
windbriskHeights :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
windbriskHeights s registry = Spec.describe s "Windbrisk Heights" $ do
  -- CR 406.3a / 305.1: the hidden card is turned face up just before it is
  -- played, so Null Chamber's name reaches it. A pair differing only in the name
  -- alice chose: Mutavault stays hidden, Goblin Piker leaves it playable.
  Spec.it s "CR 406.3a a land played from face-down exile is stopped by a prohibition naming it" $ do
    nullChamber <- S.printingOf s registry "Null Chamber"
    (_, land, legWith) <- attackedHeightsHiding s registry Nothing "Mutavault"
    let mutavault = CardName.MkCardName (Text.pack "Mutavault")
        naming name gs =
          let (chamber, withChamber) = S.addPermanent nullChamber S.alice gs
           in withChamber {GameState.objects = Map.adjust (\o -> o {Object.chosenNames = Map.singleton Acquisition.Own (Map.singleton S.alice (Set.singleton name))}) chamber (GameState.objects withChamber)}
        mutavaults = S.countOnBattlefieldByName mutavault S.alice
        named = legWith (naming mutavault) 0 3 land
        unnamed = legWith (naming (CardName.MkCardName (Text.pack "Goblin Piker"))) 0 3 land
    Spec.assertEqWith s "CR 305.1: named, the Mutavault is not played; otherwise it is" (fmap mutavaults [named, unnamed]) [0, 1]

-- The objects on the battlefield whose PRINTED card carries that name. Printed
-- and not projected on purpose: this is how the fixture tells Vesuva from the
-- Windbrisk Heights it copied, which no rules read may do.
battlefieldNamed :: String -> GameState.GameState -> [ObjectId.ObjectId]
battlefieldNamed name gs =
  let wanted = CardName.MkCardName (Text.pack name)
   in filter
        (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just wanted)
        (Set.toAscList (GameState.battlefield gs))

-- ONE board, replayed from alice's beginning of combat: alice plays the land
-- (hideaway 4 over a five-card library, hiding the named card), plus a Plains for
-- the {W}, three creatures to attack with, and a second turn so the land that
-- entered tapped (CR 702.75b) has untapped.
--
-- Returns the Windbrisk Heights PLACED on the battlefield (S.noSource where the
-- caller asked for no copier), the land alice PLAYED, and a leg: a change to the
-- combat-ready board, how many lands alice has already played this turn when she
-- activates (CR 305.2a), how many attackers she declares, which land she then
-- activates in her postcombat main phase, and the board that comes back.
attackedHeightsHiding ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Maybe String ->
  String ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, (GameState.GameState -> GameState.GameState) -> Natural -> Int -> ObjectId.ObjectId -> GameState.GameState)
attackedHeightsHiding s registry copier hidden = do
  heights <- S.printingOf s registry "Windbrisk Heights"
  played <- maybe (pure heights) (S.printingOf s registry) copier
  plains <- S.printingOf s registry "Plains"
  evangel <- S.printingOf s registry "Cabal Evangel"
  bolt <- S.printingOf s registry "Lightning Bolt"
  piker <- S.printingOf s registry hidden
  sentry <- S.printingOf s registry "Ogre Sentry"
  cancel <- S.printingOf s registry "Cancel"
  think <- S.printingOf s registry "Think Twice"
  let base = Setup.emptyGame S.bothPlayers
      -- The library, bottom first: the look reaches four of the five and
      -- `hiding` pins its choice to the second of them, the hidden card.
      (_, g1) = S.addLibraryCard think S.alice base
      (_, g2) = S.addLibraryCard cancel S.alice g1
      (_, g3) = S.addLibraryCard sentry S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addLibraryCard bolt S.alice g4
      (original, g6) = case copier of
        Nothing -> (S.noSource, g5)
        Just _ -> S.addPermanent heights S.alice g5
      (plainsId, g7) = S.addPermanent plains S.alice g6
      g8 = foldr (\_ g -> snd (S.addPermanent evangel S.alice g)) g7 [1 .. (3 :: Int)]
      (_, g9) = S.addHandCard played S.alice g8
      before =
        g9
          { GameState.turn =
              (GameState.turn g9)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
      -- PLAYED and not placed, so CR 603.6a's entry event fires and hideaway
      -- runs. `original` was placed, so its own hideaway never triggered.
      afterPlay = S.runPure (hiding original) before Engine.priorityLoop
      landId = case battlefieldNamed (Maybe.fromMaybe "Windbrisk Heights" copier) afterPlay of
        [only] -> only
        _ -> S.noSource
      -- CR 502.3 on a later turn: the land entered tapped, so nothing can pay its
      -- {T} until alice's next untap step. Two handoffs and that step's
      -- turn-based actions alone -- a whole turn of priority would draw this
      -- fixture's library empty (CR 104.3c).
      untapped =
        S.runPure
          S.identityAnswer
          (Engine.beginTurnOf S.alice (Engine.beginTurnOf S.bob afterPlay))
          (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
      combatReady =
        untapped
          { GameState.turn =
              (GameState.turn untapped)
                { TurnRegisters.phase = S.beginningOfCombat,
                  TurnRegisters.remaining = S.phasesAfterThroughPostcombatMain S.beginningOfCombat,
                  TurnRegisters.priority = Just S.alice
                }
          }
      leg change landsAlready attackers activated =
        S.runPure
          (activating activated plainsId)
          (S.runCombat (attackingWith attackers) (change combatReady) {GameState.landsPlayed = Map.singleton S.alice landsAlready})
          Engine.priorityLoop
  pure (original, landId, leg)

-- Declares the first `n` creatures CR 508.1a offers and takes no other action:
-- the legs of a pair differ in this number and in nothing else.
attackingWith :: Int -> Prompt.Prompt r -> r
attackingWith n p = case p of
  Prompt.DeclareAttackers _ _ candidates -> take n candidates
  _ -> S.identityAnswer p

-- Activates that permanent's one non-mana ability, pays the {W} from that Plains
-- rather than from whatever the engine offers first, and takes CR 601.3's offer.
-- The source is PINNED because the hideaway land's own "{T}: Add {W}" is a
-- candidate too, and tapping it for the mana would spend the cost's own {T}.
activating :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
activating landId plainsId p = case p of
  Prompt.ChooseAction _ _ actions ->
    Maybe.fromMaybe A.Pass (List.find (isActivationOf landId) actions)
  Prompt.ChooseManaSource _ _ _ candidates ->
    Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== plainsId) (NonEmpty.toList candidates)))
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

isActivationOf :: ObjectId.ObjectId -> A.Action -> Bool
isActivationOf oid action = case action of
  A.Activate candidate _ -> candidate == oid
  _ -> False

-- Plays whatever land the board offers, copies the named permanent when one is
-- passed, pins hideaway's choice to the SECOND card offered, and rotates the
-- batch the random order asks about.
hiding :: ObjectId.ObjectId -> Prompt.Prompt r -> r
hiding copied p = case p of
  Prompt.ChooseCopyTarget {} -> Just copied
  Prompt.ChooseCardFromAmong _ _ _ offered ->
    Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop 1 (NonEmpty.toList offered)))
  Prompt.Shuffle ids -> case ids of
    h : t -> t <> [h]
    [] -> []
  _ -> S.playLandAnswer p

-- throughPile, with CR 406.4's draw answered by the card NAMED rather than by
-- position. The answer is filtered back against the pile the engine offers, so
-- naming a card the draw was never offered cannot smuggle one in.
drawing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
drawing oid p = case p of
  Prompt.RandomObject _ -> oid
  _ -> throughPile p

-- What CR 601.2c would put in front of this player: the slot's legal set with CR
-- 406.4's substitution taken over it, which is the pair Parun.Engine.Target's
-- chooseTargets raises a prompt with.
--
-- The slot's most is its fixed count, which every slot this spec offers has.
offerTo :: PlayerId.PlayerId -> TargetSlot.TargetSlot -> GameState.GameState -> Set.Set Recipient.Recipient
offerTo pid slot gs = Target.piledOffer (Target.fixedCount slot) (Just pid) gs (Target.legalRecipients StackObjectKind.Spell (Just pid) S.noSource slot gs)

-- What each seat's view showed of exile at every prompt after CR 406.4's draw,
-- newest first, beside whether the prompt was CR 601.2g's mana window.
type Sightings = [(Bool, Set.Set ObjectId.ObjectId, Set.Set ObjectId.ObjectId)]

-- throughPile, recording what both seats' views show at each prompt raised
-- after the draw out of the pile -- the window CR 406.4 keeps the card hidden
-- through. The draw itself is the RandomObject prompt.
watchingPayment :: Asked.Asked GameState.GameState r -> State.State (Maybe Sightings) r
watchingPayment asked = do
  let p = Asked.prompt asked
      gs = Asked.game asked
      manaWindow = case p of
        Prompt.ChooseManaSource {} -> True
        _ -> False
  State.modify' (fmap ((manaWindow, facesShown S.alice gs, facesShown S.bob gs) :))
  case p of
    Prompt.RandomObject _ -> State.put (Just [])
    _ -> pure ()
  pure (throughPile p)

-- The exiled cards whose face this seat's PlayerView shows (CR 406.3, 701.20a).
facesShown :: PlayerId.PlayerId -> GameState.GameState -> Set.Set ObjectId.ObjectId
facesShown pid gs =
  Set.fromList
    [ ObjectView.object view
    | seat <- PlayerView.Type.seats (PlayerView.viewFor pid gs),
      view <- SeatView.exile seat,
      Maybe.isJust (ObjectView.face view)
    ]

-- The two halves of CR 406.4's last sentence over one cast or activation off
-- pileBoardUncast: nothing face down is shown to either seat at any prompt
-- after the draw, and once paid for both seats witness a reveal of the drawn
-- card and not of the other card of its pile. The reveal lasts CR 701.20a's
-- brief time, so with the spell or ability still on the stack neither seat is
-- shown a face.
revealedOncePaid :: (Monad m) => Spec.Spec m n -> CardName.CardName -> CardName.CardName -> GameState.GameState -> GameState.GameState -> Maybe Sightings -> m ()
revealedOncePaid s drawn other before after seen = do
  let named name = List.find (\oid -> namesOf [oid] after == Set.singleton name) (faceDownExiled after)
      sightings = Maybe.fromMaybe [] seen
  Spec.assertEqWith s "while it is paid for, neither seat sees a face-down card" [(a, b) | (_, a, b) <- sightings, not (Set.null a && Set.null b)] []
  case (named drawn, named other) of
    (Just drawnId, Just otherId) -> do
      Spec.assertEqWith s "once paid for, alice witnesses the drawn card revealed and not the rest of its pile" (revealsWitnessed S.alice after) [drawnId]
      Spec.assertEqWith s "and so does bob" (revealsWitnessed S.bob after) [drawnId]
      Spec.assertEqWith s "the reveal is over while it is still on the stack" (facesShown S.alice after <> facesShown S.bob after) Set.empty
      -- Proxies, AFTER the behaviour so neither can absorb a mutation: the
      -- draw was asked, a mana window followed it, and nothing showed before.
      Spec.assertBool s (any (\(window, _, _) -> window) sightings) "a mana window was raised after the draw"
      Spec.assertEqWith s "before the cast, neither seat sees a face-down card" (facesShown S.alice before <> facesShown S.bob before) Set.empty
      Spec.assertBool s (drawnId /= otherId) "the pile's two cards are two objects"
    _ -> Spec.assertFailure s "both cards of the pile should still be in exile face down"

-- The cards this seat witnessed revealed this turn (CR 701.20a), in log order.
revealsWitnessed :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
revealsWitnessed pid gs =
  [Revealed.card r | GameEvent.Revealed r <- fmap LoggedEvent.event (PlayerView.Type.witnessed (PlayerView.viewFor pid gs))]

-- The first draw a slot may name out of a pile, which is the only one a
-- single-target slot is offered (CR 406.4).
firstDraw :: Pile.Pile -> Recipient.Recipient
firstDraw pile = Recipient.ToPile PileDraw.MkPileDraw {PileDraw.pile = pile, PileDraw.ordinal = 1}

-- CR 613.7d's stamp, which is what names a foretold card's pile.
timestampOf :: ObjectId.ObjectId -> GameState.GameState -> Timestamp.Timestamp
timestampOf oid gs = maybe (Timestamp.MkTimestamp 0) Object.timestamp (Game.lookupObject oid gs)

-- alice casts Ignorant Bliss with Goblin Piker and Lightning Bolt in hand and a
-- face-up Riftsweeper of bob's already in exile, then holds her copy of
-- Synthetic Blind Reclamation and the {1}{U} it costs. Every name is its own
-- card, so the two cards of the pile can be told apart in the zone each ends up
-- in. Returns the name of the pile's last card, which throughPile's draw names,
-- the name of its other card, the spell, and the board she has priority on.
pileBoardUncast ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (CardName.CardName, CardName.CardName, ObjectId.ObjectId, GameState.GameState)
pileBoardUncast s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  sentry <- S.printingOf s registry "Ogre Sentry"
  riftsweeper <- S.printingOf s registry "Riftsweeper"
  reclamation <- S.printingOf s registry "Synthetic Blind Reclamation"
  let (g1, blissId) = S.handOne bliss (S.landsFor island S.alice 2 (S.landsInPlay mountain 2))
      (_, g2) = S.addHandCard piker S.alice g1
      (_, g3) = S.addHandCard bolt S.alice g2
      (_, g4) = S.addLibraryCard sentry S.alice g3
      (_, g5) = S.addExiledCard riftsweeper S.bob g4
      blissed = S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice blissId)) Engine.priorityLoop
      (spellId, g6) = S.addHandCard reclamation S.alice blissed
      before =
        g6
          { GameState.turn =
              (GameState.turn g6)
                { TurnRegisters.activePlayer = S.alice,
                  TurnRegisters.phase = Phase.PrecombatMain,
                  TurnRegisters.priority = Just S.alice
                }
          }
      -- WHICH card is which is read off the board rather than assumed: CR 400.7
      -- mints a fresh incarnation as each card is exiled, so the ids the pile
      -- holds are not the ids the hand held, and it is the pile's own order that
      -- throughPile's draw answers with.
      nameAt i = Set.toList (namesOf (take 1 (drop i (faceDownExiled before))) before)
   in pure $ case (nameAt 1, nameAt 0) of
        ([last_], [first_]) -> (last_, first_, spellId, before)
        _ -> (S.printingName bolt, S.printingName piker, spellId, before)

-- Every object on the stack resolved, with nobody taking an action.
resolveAll :: GameState.GameState -> GameState.GameState
resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop

-- The face-down exiled cards of one pile, in the order Parun.Engine.Target's
-- pileMembers offers them -- ascending by object id, which is Set.toList's.
membersOfPile :: GameState.GameState -> Pile.Pile -> [ObjectId.ObjectId]
membersOfPile gs pile = filter (\oid -> Exile.pileOf oid gs == Just pile) (faceDownExiled gs)

-- Answers CR 601.2c with the PILE -- filtered out of the offer rather than built,
-- so a candidate the engine never offered cannot be smuggled in -- and answers CR
-- 406.4's draw with the LAST card of the pile, which is the one answer a draw
-- that took the first would not produce.
throughPile :: Prompt.Prompt r -> r
throughPile p =
  let isPile recipient = case recipient of
        Recipient.ToPile _ -> True
        _ -> False
   in case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter isPile . snd) sets
        Prompt.RandomObject members -> NonEmpty.last members
        _ -> S.identityAnswer p

aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((==) (Just oid) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- The card names in one player's copy of a zone, and the same over a list of
-- objects -- a shuffle mints a new incarnation (CR 400.7), so a card that moved
-- is followed by name rather than by object id.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> Set.Set CardName.CardName
namesIn zone pid gs = namesOf (Game.zoneMembers zone pid gs) gs

namesOf :: [ObjectId.ObjectId] -> GameState.GameState -> Set.Set CardName.CardName
namesOf oids gs = Set.fromList (Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) oids)

-- The distinct piles the face-down exiled cards of a board are in (CR 406.4).
pilesIn :: GameState.GameState -> [Pile.Pile]
pilesIn gs = Set.toList (Set.fromList (Maybe.mapMaybe (\oid -> Exile.pileOf oid gs) (faceDownExiled gs)))

-- The exiled cards CR 406.3's rider left face down.
faceDownExiled :: GameState.GameState -> [ObjectId.ObjectId]
faceDownExiled gs = filter (\oid -> any Object.exiledFaceDown (Game.lookupObject oid gs)) (Set.toList (GameState.exile gs))

-- alice casts Ignorant Bliss off two Mountains with Goblin Piker and Lightning
-- Bolt in hand, and one Ogre Sentry already exiled face up under bob. Her
-- library is stocked so the delayed ability's draw has a card and CR 104.3c
-- never fires.
castBliss :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castBliss s registry = do
  bliss <- S.printingOf s registry "Ignorant Bliss"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  bolt <- S.printingOf s registry "Lightning Bolt"
  sentry <- S.printingOf s registry "Ogre Sentry"
  let (g1, blissId) = S.handOne bliss (S.landsInPlay mountain 2)
      (_, g2) = S.addHandCard piker S.alice g1
      (_, g3) = S.addHandCard bolt S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (_, g5) = S.addExiledCard sentry S.bob g4
  pure (S.runPure S.identityAnswer (S.runPure S.identityAnswer g5 (S.cast S.alice blissId)) Engine.priorityLoop)

-- The end step of alice's turn, settled and resolved -- Parun.TriggerSpec's
-- delayed-ability group takes the same route.
endStep :: GameState.GameState -> GameState.GameState
endStep gs =
  let phase = Phase.Ending EndingStep.EndStep
      began =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan phase S.alice))
          ( gs
              { GameState.turn =
                  (GameState.turn gs)
                    { TurnRegisters.phase = phase
                    }
              }
          )
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer began Engine.settleForPriority) Engine.priorityLoop

-- The exiled cards CR 406.3's default left face up.
faceUpExiled :: GameState.GameState -> [ObjectId.ObjectId]
faceUpExiled gs = filter (\oid -> not (any Object.exiledFaceDown (Game.lookupObject oid gs))) (Set.toList (GameState.exile gs))

-- The objects in a zone still carrying CR 406.3's face-down flag, which outside
-- exile must always be none.
concealedIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
concealedIn zone pid gs = filter (\oid -> any Object.exiledFaceDown (Game.lookupObject oid gs)) (Game.zoneMembers zone pid gs)

-- The one target slot of the one triggered ability a printing declares --
-- Parun.TargetSpec's triggerTargetSlot, kept local so this group reads Riftsweeper
-- out of the committed card rather than a hand-built TargetSlot.
triggerTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
triggerTargetSlot printing = case Face.triggeredAbilities (S.combinedFace printing) of
  [ability] -> case Map.elems (Modal.allTargetSlots (TriggeredAbility.modal ability)) of
    [only] -> Just only
    _ -> Nothing
  _ -> Nothing
