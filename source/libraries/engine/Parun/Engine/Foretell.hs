-- Rule 702.143 in the one voice the rest of the engine cannot supply for itself:
-- CR 116.2h's special action that pays {2} to exile a card with foretell from a
-- hand FACE DOWN, and the stamp that makes the exiled card a FORETOLD one. CR
-- 702.143d's other route into that stamp -- a spell or ability that makes an
-- exiled card foretold, and may give it a foretell cost -- is an Effect opcode
-- and belongs to the open half, so its arm sits in Parun.Engine.Resolve and calls
-- becomeForetold here.
--
-- The rule's other half lives where every other casting question does. CR
-- 702.143a's permission -- "they may cast that card after the current turn has
-- ended by paying any foretell cost it has" -- is read by
-- Parun.Engine.Cast.permitsCastFromExile and priced by Parun.Engine.Cost.costsFor,
-- off the Object.foretold stamp this module writes. What is left is an action a
-- player takes, and an action needs a place to be offered from and performed in.
-- Parun.Engine.Plot is the same module for rule 702.170, and this one is written
-- to its shape.
--
-- THE INVARIANT: rule 702 is part of the rulebook, so reading Keyword.Foretell
-- here is the same closed-half act as reading a Phase. This module never asks
-- which CARD is being foretold.
module Parun.Engine.Foretell where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Numeric.Natural (Natural)
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Keyword as Keyword
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Turn as Turn
import Parun.Types.Cost (Cost)
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.CostAdjustments as CostAdjustments
import qualified Parun.Types.CostedSpecialAction as CostedSpecialAction
import qualified Parun.Types.EntryRiders as EntryRiders
import Parun.Types.ForetellCost (ForetellCost)
import qualified Parun.Types.ForetellCost as ForetellCost
import Parun.Types.Game (Game)
import qualified Parun.Types.GameEvent as GameEvent
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import Parun.Types.Keyword (Keyword)
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.PaymentSubject as PaymentSubject
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import Parun.Types.PlayerId (PlayerId)
import Parun.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import qualified Parun.Types.ProjectedCharacteristics as PC
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

-- CR 116.2h: what the special action costs -- "may pay {2} and exile that card
-- face down".
--
-- Minted here rather than read off Keyword.Foretell, which is the whole
-- difference from Parun.Engine.Plot.plotCostsOf: rule 116.2h fixes this amount for
-- every printing, and the keyword's own payload is the later CAST's cost
-- (Parun.Engine.Keyword.foretellCosts). Parun.Engine.Cost.faceDownCost is the same
-- shape one rule over -- an amount rule 702.37a states rather than any card.
actionCost :: Cost Keyword
actionCost =
  Cost.Type.MkCost
    { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
      Cost.Type.components = []
    }

-- Does this object have foretell at all, and with what payloads? Empty for a
-- hand member with no card behind it -- a token, an ability.
--
-- Read through the projection (CR 613.1f), since the ability functions in the
-- hand (CR 702.143a) and an effect may grant it there -- Dream Devourer's "each
-- nonland card in your hand without foretell has foretell". Parun.SpecialActionSpec's
-- Devourer pair proves it.
--
-- Returns the CAST costs the keywords carry, though this module spends it only
-- as foretell's stamp: what it answers for canForetell is "is the keyword
-- there".
--
-- `offBoard` is a Projection.projectOffBoard map of `gs`, or empty to project
-- the card afresh (#9709).
foretellCostsOf :: Map.Map ObjectId ProjectedCharacteristics -> ObjectId -> GameState -> [ForetellCost Keyword]
foretellCostsOf offBoard oid gs = case Game.cardOfHandMember oid gs of
  Nothing -> []
  Just _ -> Keyword.foretellCosts (Map.keysSet (PC.keywords (Projection.projectEachGiven offBoard oid gs)))

-- CR 702.143a / 116.2h: may this player foretell this card right now? Three
-- conjuncts, each a clause of the rule:
--
--   * the card is in THIS PLAYER'S HAND with foretell on it ("exile a card with
--     foretell from their hand"), which foretellCostsOf and the zone test settle
--     together;
--   * the window is THIS PLAYER'S OWN TURN -- "any time a player has priority
--     during their turn" -- or any player's under a grant (Cosmos Charger).
--     Nothing more: rule 702.143a asks for neither a main phase nor an empty
--     stack, so this is deliberately NOT Parun.Engine.Turn.sorcerySpeedWindow,
--     which CR 702.170a's plot and CR 116.2m's unlock both are.
--     Parun.SpecialActionSpec pairs the two boards that prove the difference.
--   * the {2}, as an effect changes it, is payable. An action the player
--     cannot take is not offered, which is Parun.Engine.Action.legalActions'
--     posture throughout. It is paid FOR the
--     card (PaymentSubject.Foretelling), which Karfell Harbinger's "only to
--     foretell a card" asks about; the scenario "CR 106.6 / 116.2h Karfell
--     Harbinger's mana foretells a card ..." proves it.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments, for the reason Plot.canPlot gives: that rule totals the cost of a
-- spell being cast or an ability being activated, and a special action is
-- neither (#90). What an effect changes about foretelling (CR 118.7) is asked
-- instead, `foretellCost`'s adjustments, any total they reach sufficing.
-- data/scenarios/cosmos-charger-foretells-on-an-opponents-turn-for-one-less
-- proves both grants.
--
-- The PRIORITY clause has no conjunct, for the reason CR 116.2a's land play has
-- none: legalActions is asked only of the priority holder.
canForetell :: Map.Map ObjectId ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> Bool
canForetell offBoard pid oid gs =
  not (null (foretellCostsOf offBoard oid gs))
    && elem oid (Game.zoneMembers Zone.Hand pid gs)
    && (Turn.hasTurnRunning gs pid || PlayerEffect.mayForetellDuringAnyTurn pid gs)
    && let (adjustments, cost) = foretellCost pid oid gs
           payableWith manaCost = Cost.canPay (PaymentSubject.Foretelling oid) pid oid cost {Cost.Type.mana = manaCost} gs
        in case Cost.Type.mana cost of
             Nothing -> payableWith Nothing
             Just manaCost -> any (payableWith . Just) (Cost.totalManas adjustments manaCost)

-- CR 118.7 / 116.2h: what an effect changes about foretelling this card (Cosmos
-- Charger's "Foretelling cards from your hand costs {1} less"), gathered over
-- the rows whose subject is CostSubject.SpecialActions Foretell, and the cost
-- with their added components -- Plot.plotCost's shape, asked of the card in
-- hand before the exile.
foretellCost :: PlayerId -> ObjectId -> GameState -> (CostAdjustments.CostAdjustments, Cost Keyword)
foretellCost pid oid gs =
  let adjustments = PlayerEffect.specialActionCostAdjustments CostedSpecialAction.Foretell pid oid gs
   in (adjustments, Cost.plusComponents adjustments actionCost)

-- Every card this player may foretell right now -- what Action.Foretell is built
-- from, and the shape Plot.plottable has.
foretellable :: Map.Map ObjectId ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
foretellable offBoard pid gs = filter (\oid -> canForetell offBoard pid oid gs) (Game.zoneMembers Zone.Hand pid gs)

-- CR 702.143a: pay {2} and exile the card from hand face down, leaving it a
-- foretold card.
--
-- REJECT-NOT-REPAIR, and the payment first, for the reasons Parun.Engine.Plot.plot
-- states: a payment that fails restores the state from before it was attempted
-- and the card stays in hand.
--
-- FACE DOWN, which is the whole difference from CR 702.170a's plot: rule 702.143a
-- says "exile a card with foretell from their hand FACE DOWN", so the move
-- carries CR 406.3's rider rather than that rule's face-up default. It goes
-- through Event.changeZoneEntering because that is the door the rider is read
-- from, so the card is never face up in exile for an instant.
--
-- The stamp is written onto the id the move RETURNS and never onto `oid`, for
-- Plot.plot's reason: CR 400.7 mints a fresh incarnation in exile.
--
-- CR 702.143a's "that player may look at that card as long as it remains in
-- exile" is read off this stamp rather than stored beside it, by
-- Parun.Engine.Exile.mayLookAt -- so the owner of a foretold card may NAME it out
-- of exile where another player may only pick the pile it is in (CR 406.4).
-- The exile scenario "CR 406.4 the owner of a foretold card shuffles it out of
-- exile and an opponent aiming at it by name gets the face-up one" is the pair
-- that proves it.
--
-- CR 702.143e is what makes each foretold card a pile of its own in
-- Parun.Engine.Exile.pileOf, and a seat's view lists exile by id, which CR
-- 400.7's fresh incarnation mints in arrival order, and carries the stamp's
-- granted cost to a seat that may look at the card
-- (Parun.Engine.PlayerView.objectView). Parun.PlayerViewSpec's "CR 702.143e a
-- foretold card shows the foretell cost Dream Devourer gave it" proves it.
foretell :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> ObjectId -> Game ()
foretell perform pid oid = do
  before <- State.get
  Monad.when (canForetell Map.empty pid oid before) $ do
    let (adjustments, cost) = foretellCost pid oid before
    announced <- Cost.announceReductions pid oid before cost adjustments
    paid <- Cost.payAction perform before (PaymentSubject.Foretelling oid) 0 pid oid (Cost.totalWith announced cost)
    Monad.forM_ paid $ \_ -> do
      -- One stamp per arrival, Parun.Engine.Plot's reason: the funnel answers
      -- with more than one only for a melded permanent leaving the battlefield
      -- (CR 713.8), and this action exiles a card from a hand.
      exiled <- Event.changeZoneEntering (ManaAbilityPerformer.settle perform) oid Zone.Exile LibraryPosition.defaultValue riders Nothing
      Monad.forM_ exiled (State.modify' . becomeForetold (Maybe.listToMaybe (Maybe.mapMaybe reductionOf (foretellCostsOf Map.empty oid before))))
      -- CR 702.143c: "foretelling a card" is this special action.
      Monad.unless (null exiled) (State.modify' (Event.recordEvent (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.Foretell pid))))

-- A granted foretell stops applying once the card leaves the hand, yet the card
-- keeps the cost it was given (Dream Devourer's ruling): so a reduction off the
-- card's own mana cost is stamped onto the exiled card, where
-- Parun.Engine.Cost.grantedForetellCost settles it per face (CR 712.8b).
--
-- A STATED cost is not stamped: the exiled card's own face still prints it.
-- What would refute that is an effect granting a stated foretell cost in a hand;
-- Scryfall @o:"foretell" -kw:foretell@, 2026-09-26, names none (Dream Devourer
-- and Bohn, Beguiling Balladeer both grant the reduction). Both grant the same
-- {2}, so the first reduction is the only one.
reductionOf :: ForetellCost Keyword -> Maybe ManaCost.ManaCost
reductionOf payload = case payload of
  ForetellCost.Stated _ -> Nothing
  ForetellCost.ManaCostReducedBy amount -> Just amount

-- CR 406.3's rider and nothing else: every other rider is battlefield-only, and
-- this move names exile.
--
-- The funnel's instantiation, a settled count rather than a Quantity: nothing
-- here is resolving, so there is no CR 608.2h moment to evaluate one at.
riders :: EntryRiders.EntryRiders Natural ability
riders =
  EntryRiders.defaultValue {EntryRiders.exiledFaceDown = True}

-- "It becomes a foretold card" -- the stamp and CR 702.143d's granted cost
-- together, which is the WHOLE of what becoming foretold is. Parun.Engine.Plot's
-- becomePlotted is this function one rule over.
--
-- The stamp is the TURN it became one, which is what CR 702.143a's "after the
-- current turn has ended" is compared against, and it is read AFTER the move for
-- Plot.stamp's reason: nothing in a zone change ends a turn, so the two numbers
-- agree.
--
-- Two routes reach it and the rulebook gives them one meaning: CR 116.2h's
-- special action above, which grants no cost, and CR 702.143d's "if an effect
-- states that a card in exile becomes foretold" (Parun.Engine.Resolve's
-- Effect.MakeForetold arm), which may. Both land here so neither can drift from
-- the other.
--
-- What is written is the REDUCTION the effect stated and never a cost: rule
-- 702.143d's cost is per FACE (CR 712.8b), and the face is not known until the
-- cast is proposed, so Parun.Engine.Cost.grantedForetellCost settles it there.
-- Object.foretellCostReduction says why this field is that shape.
--
-- Takes an ObjectId and an amount of mana, never a card: this module's invariant
-- is that it never asks which card is being foretold, so the opcode's arm hands
-- it CR 400.7's exiled incarnation exactly as the special action does.
--
-- No GameEvent rides along, where Plot.becomePlotted records one: CR 702.143c's
-- "foretelling a card" is the special action alone, so `foretell` above records
-- GameEvent.PlayerActed and CR 702.143d's route records nothing.
becomeForetold :: Maybe ManaCost.ManaCost -> ObjectId -> GameState -> GameState
becomeForetold reduction newId gs =
  gs
    { GameState.objects =
        Map.adjust
          (\o -> o {Object.foretold = Just (TurnRegisters.turnNumber (GameState.turn gs)), Object.foretellCostReduction = reduction})
          newId
          (GameState.objects gs)
    }
