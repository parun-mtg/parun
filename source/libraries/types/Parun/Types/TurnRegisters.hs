module Parun.Types.TurnRegisters where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.AbilityTriggered as AbilityTriggered
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CastFromZone as CastFromZone
import qualified Parun.Types.Combat as Combat
import qualified Parun.Types.EndTurnSignal as EndTurnSignal
import qualified Parun.Types.EventGroup as EventGroup
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.LastKnown as LastKnown
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.ModifiedRoll as ModifiedRoll
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PastActivation as PastActivation
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Timestamp as Timestamp
import qualified Parun.Types.TurnResume as TurnResume

-- | CR 500 / 807.4b: a turn's position and its per-turn tallies, held whole as
-- GameState.turn while worked and parked on GameState.parkedTurns, under its
-- marker's number, while another Grand Melee turn is.
data TurnRegisters = MkTurnRegisters
  { activePlayer :: PlayerId.PlayerId,
    phase :: Phase.Phase,
    -- | CR 500: the steps still scheduled this turn, in order, `phase` being the
    -- one in progress; CR 508.8 drops steps and CR 510.4, 500.8 and 500.9
    -- splice them in. Refilled from `Turn.allPhases` at handoff.
    remaining :: Seq.Seq Phase.Phase,
    -- | CR 405.1 / 807.5: the stack, top first; in Grand Melee its turn
    -- marker's own, every stack being Parun.Engine.Game.allStacks.
    stack :: [ObjectId.ObjectId],
    -- | CR 610.3a / 610.3b: the first event group whose events happened after
    -- each object was put on the stack, or for a triggered ability after it
    -- triggered. Written by Parun.Engine.Game.putOnStack, cleared with `events`.
    stackedIn :: Map.Map ObjectId.ObjectId EventGroup.EventGroup,
    priority :: Maybe PlayerId.PlayerId,
    -- | CR 117.4 / 805.5b: the players who have passed in succession since the
    -- last action, the stack's resolution or the step's first grant.
    passed :: Set.Set PlayerId.PlayerId,
    -- | CR 508/509. Lives for one combat phase; cleared at CR 511.
    combat :: Combat.Combat,
    -- | CR 608.2i: what happened this turn, in order, each entry stamped with
    -- its EventGroup (CR 608.2f's single event). Cleared at turn handoff
    -- (Engine.handoffTurn), never by a reader. A new reader of it as history
    -- owes its construct's arm in Parun.Engine.Interchangeable.Mentions a
    -- Parun.Types.HistoryRead (CR 733.3).
    --
    -- Not implemented: in Grand Melee this is the worked turn's log, which a
    -- static ability's "this turn" reads whichever turn's question is being
    -- asked (#6538).
    events :: Seq.Seq LoggedEvent.LoggedEvent,
    -- | CR 117.5: how far the trigger scan has consumed the event log.
    scannedThrough :: Natural.Natural,
    -- | How far the state-based action check has consumed the event log --
    -- CR 704.5h's and CR 903.9a's "since the last time state-based actions were
    -- checked".
    damageScannedThrough :: Natural.Natural,
    turnNumber :: Natural.Natural,
    -- | CR 500.7 / 103.1: while an extra turn is under way, the seat the
    -- ordinary turn order resumes from; Nothing on an ordinary turn.
    turnAnchor :: Maybe PlayerId.PlayerId,
    -- | CR 500.7: the ExtraTurn.createdAt of the extra turn under way; Nothing
    -- on an ordinary turn.
    extraTurnUnderWay :: Maybe Timestamp.Timestamp,
    -- | CR 500.12 / 502.1: every permanent as this turn began, phased-out ones
    -- being none (CR 702.26b); Nothing until Parun.Engine.Engine.runStep files
    -- it at the turn's first step, Engine.beginTurn having cleared the last
    -- turn's. An unforced thunk over that board, whose own copy of this field is
    -- Nothing, so only a reader projects it, Reversal.withoutAnnouncement's
    -- comparison among them.
    permanentsAsTurnBegan :: Maybe (Map.Map ObjectId.ObjectId LastKnown.LastKnown),
    -- | CR 725.1: raised while an effect that ends the turn unwinds through the
    -- frames running the ended step; Engine.runStep lowers it as CR 725.1d's
    -- cleanup step begins.
    endTurnSignal :: EndTurnSignal.EndTurnSignal,
    -- | CR 121.1: the cards each player has drawn this turn, in order, each as the
    -- incarnation that reached the hand; its length is the ordinal stamped onto
    -- GameEvent.Drew. Cleared for every player at turn handoff, and after CR
    -- 103.5's opening hands.
    drawsThisTurn :: Map.Map PlayerId.PlayerId (Seq.Seq ObjectId.ObjectId),
    -- | CR 508.1a-d: the creatures that could have been declared as attackers
    -- in a declare attackers step this turn, written as each such step's
    -- declaration begins (Parun.Engine.Combat.declareAttackers) and cleared at
    -- turn handoff. Filter.CouldAttackThisTurn reads it.
    couldAttackThisTurn :: Set.Set ObjectId.ObjectId,
    -- | CR 111.1 / 614.1a: the players an effect or cost would have created one
    -- or more tokens for this turn, whether or not any was created (CR 111.5);
    -- cleared at turn handoff. Written by Parun.Engine.Event.createTokens as its
    -- CR 608.2f action ends, read by Parun.Types.TokenPattern.firstEachTurn.
    wouldCreateTokens :: Set.Set PlayerId.PlayerId,
    -- | CR 602.5b: which abilities of which objects have been activated this
    -- turn, read by ActivationRestriction.TimesEachTurn alone; cleared at turn
    -- handoff, which is the whole of "each turn". Keyed by the SOURCE object and
    -- then by the ability, since a permanent may print the rider on one of two
    -- abilities (Locust Swarm). The ability BY VALUE and counted, for CR
    -- 602.5c's identically worded twins, and as printed (CR 612.1), as
    -- Object.activatedOnce is.
    activatedThisTurn :: Map.Map ObjectId.ObjectId (Map.Map (ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)) Natural.Natural),
    -- | CR 602.2: every activated ability begun this turn, oldest first, as
    -- an activation's Parun.Types.CostModifier.onlyNth, Quantity.TimesThisTurn and
    -- Quantity.ActivationsThisTurn ask after it; cleared at turn handoff. Written by
    -- Parun.Engine.ActivationRestriction.logActivation. A
    -- new reader owes its construct's arm in Parun.Engine.Interchangeable.Mentions
    -- HistoryRead.Activations.
    activationsThisTurn :: Seq.Seq PastActivation.PastActivation,
    -- | CR 601.3: which once-each-turn permissions have been used this turn, by
    -- a cast or (a Play-verb one) a land play, read only for a budgeted
    -- PermissionLimit; cleared at turn handoff, which is the whole of "each
    -- turn". Keyed by the object granting the
    -- permission and then by the permission itself, so two copies of one
    -- enchantment grant two budgets and one permanent printing two permissions
    -- spends them separately -- the grain activatedThisTurn above uses for CR
    -- 602.5b's activation-side twin, and for that rule's reason: the budget is
    -- the ability's and survives a change of control.
    --
    -- The permission BY VALUE, so a CR 612.1 word swap that changed this
    -- permission's own filter mid-turn would leave a spent budget unfound --
    -- the caveat activatedThisTurn's key carries too. No text change reaches
    -- one: Parun.Engine.Projection.Rewrite swaps subtype words, and no budgeted
    -- permission in data/cards/ names a subtype.
    castPermissionsUsedThisTurn :: Map.Map ObjectId.ObjectId (Set.Set CastFromZone.CastFromZone),
    -- | CR 706.2: which budgeted die-roll modifiers have been taken this turn,
    -- read by a Parun.Types.ModifiedRoll whose limit is
    -- PermissionLimit.OnceEachTurn (Night Shift of the Living Dead); cleared at
    -- turn handoff. Keyed as castPermissionsUsedThisTurn above and for its reason.
    rollModifiersUsedThisTurn :: Map.Map ObjectId.ObjectId (Set.Set ModifiedRoll.ModifiedRoll),
    -- | CR 603.2h: each resolution this turn of a triggered ability whose
    -- "Do this only once each turn" instruction was taken
    -- (Parun.Types.SpentOn's TakingInstruction), as the record CR 603.3b's log
    -- carries; cleared at turn handoff. A sequence for triggeredThisGame's
    -- reason: Parun.Engine.Event.withinTriggerLimit counts it.
    instructionsTakenThisTurn :: Seq.Seq AbilityTriggered.AbilityTriggered,
    -- | CR 807.4b: where this turn picks up when the
    -- scheduler next works it; AtStepStart outside Grand Melee.
    turnResume :: TurnResume.TurnResume
  }
  deriving (Eq, Ord, Show)
