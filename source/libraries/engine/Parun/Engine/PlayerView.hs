-- One seat's view of a game: built field by field from the state, never as a
-- redacted copy of it, so a field the state gains reaches no seat, in process
-- or over the line, until it is added here on purpose.
module Parun.Engine.PlayerView where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.InherentTriggerSource as InherentTriggerSource
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.Source as Source
import qualified Parun.Types.StackedAbility as StackedAbility
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.WhichTurn as WhichTurn
import qualified Parun.Types.Zone as Zone

-- | CR 400.2 / 724.4: everything `pid` may see of the game.
viewFor :: PlayerId.PlayerId -> GameState.GameState -> PlayerView.PlayerView
viewFor pid gs =
  PlayerView.MkPlayerView
    { PlayerView.viewer = pid,
      PlayerView.game = MatchScope.gameNumber (GameState.match gs),
      PlayerView.turn = TurnRegisters.turnNumber (GameState.turn gs),
      PlayerView.phase = TurnRegisters.phase (GameState.turn gs),
      PlayerView.active = TurnRegisters.activePlayer (GameState.turn gs),
      PlayerView.priority = TurnRegisters.priority (GameState.turn gs),
      PlayerView.seats = Maybe.mapMaybe (seatView pid gs) (GameState.seating gs),
      PlayerView.battlefield = Maybe.mapMaybe (objectView pid gs) (Foldable.toList (GameState.battlefield gs)),
      PlayerView.stack = Maybe.mapMaybe (objectView pid gs) (TurnRegisters.stack (GameState.turn gs)),
      PlayerView.parkedStacks = fmap (Maybe.mapMaybe (objectView pid gs) . TurnRegisters.stack) (GameState.parkedTurns gs),
      PlayerView.witnessed = witnessedIn (TurnRegisters.events (GameState.turn gs)),
      PlayerView.witnessedLastTurn = witnessedIn (Game.turnLog WhichTurn.LastTurn gs),
      PlayerView.result = GameState.result gs,
      PlayerView.undone = Map.lookup pid (Bookkeeping.undone (GameState.bookkeeping gs))
    }
  where
    witnessedIn :: Seq.Seq LoggedEvent.LoggedEvent -> [LoggedEvent.LoggedEvent]
    witnessedIn = filter (\logged -> Visibility.witnessed pid logged == Visibility.Type.Visible) . Foldable.toList

-- | CR 400.2: a hidden zone by its size and the members `pid` may see; a public
-- one in full. A library card whose id reached a seat is re-minted when
-- shuffled (Parun.Engine.Event.forgetShown), so no seat follows it through.
-- The draft notes go to every seat (CR 905.2b).
seatView :: PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> Maybe SeatView.SeatView
seatView pid gs seat = do
  player <- Map.lookup seat (GameState.players gs)
  let members zone = Game.zoneMembers zone seat gs
      public zone = Maybe.mapMaybe (objectView pid gs) (members zone)
      hidden zone = Maybe.mapMaybe (objectView pid gs) (filter (\oid -> Visibility.sees pid oid gs == Visibility.Type.Visible) (members zone))
  pure
    SeatView.MkSeatView
      { SeatView.player = seat,
        SeatView.life = Player.life player,
        SeatView.counters = Player.counters player,
        SeatView.handSize = Natural.length (members Zone.Hand),
        SeatView.hand = hidden Zone.Hand,
        SeatView.librarySize = Natural.length (members Zone.Library),
        SeatView.library = hidden Zone.Library,
        SeatView.graveyard = public Zone.Graveyard,
        SeatView.exile = public Zone.Exile,
        SeatView.manaPool = Map.findWithDefault (Mana.MkMana []) seat (GameState.manaPool gs),
        SeatView.draftNotes = Player.draftNotes player
      }

-- | CR 110.5: an object's status for anyone; its characteristics (as projected,
-- CR 613) for a seat that may see them, which for a face-down permanent or
-- spell is every seat (CR 708.2); the card behind a face-down object for a
-- seat that may look at it (CR 406.3, 708.5), and the foretell cost it was
-- given with it (CR 702.143e); what an ability came from; and what a stack
-- object targets, which CR 601.2c / 602.2b / 603.3d announce to every seat.
objectView :: PlayerId.PlayerId -> GameState.GameState -> ObjectId.ObjectId -> Maybe ObjectView.ObjectView
objectView pid gs oid = do
  object <- Game.lookupObject oid gs
  let seen = Visibility.sees pid oid gs == Visibility.Type.Visible
      public = Facing.isFaceDown (Object.facing object) && (Game.zoneOf oid gs == Just Zone.Battlefield || Game.zoneOf oid gs == Just Zone.Stack)
      faceDown = Facing.isFaceDown (Object.facing object) || Object.exiledFaceDown object
  pure
    ObjectView.MkObjectView
      { ObjectView.object = oid,
        ObjectView.owner = Object.owner object,
        ObjectView.controller = Projection.View.controllerOf oid gs,
        ObjectView.tapped = Object.tapped object,
        ObjectView.damage = Object.damage object,
        ObjectView.counters = Object.counters object,
        ObjectView.attachedTo = Object.attachedTo object,
        ObjectView.characteristics =
          if public || (seen && not faceDown)
            then Just (Projection.project oid gs)
            else Nothing,
        ObjectView.face =
          if seen && faceDown
            then Just (Projection.projectShown oid gs)
            else Nothing,
        ObjectView.foretellCostReduction =
          if seen
            then Object.foretellCostReduction object
            else Nothing,
        ObjectView.ability = abilityView pid gs oid,
        ObjectView.targets = Set.toList (Projection.View.targetsOfStackObject gs oid)
      }

-- | CR 113.7a / 602.2a: an ability on the stack by its source, where `pid` may
-- know that object: one in a public zone, one `pid` sees in a hidden zone, or
-- one that has left, which the seat can name only by what it witnessed; and
-- which ability it is, which the stack shows every seat (CR 400.2).
abilityView :: PlayerId.PlayerId -> GameState.GameState -> ObjectId.ObjectId -> Maybe AbilityView.AbilityView
abilityView pid gs oid = do
  object <- Game.lookupObject oid gs
  stacked <-
    if Object.zone object /= Zone.Stack
      then Nothing
      else case Object.source object of
        Source.OfAbility a -> Just (StackedAbility.Activated (ActivatedAbilitySource.ability a))
        Source.OfTrigger t -> Just (StackedAbility.Triggered (TriggeredAbilitySource.ability t))
        Source.OfInherentTrigger t -> Just (StackedAbility.Triggered (InherentTriggerSource.ability t))
        Source.OfCard _ -> Nothing
        Source.OfMeld _ -> Nothing
        Source.OfMerge _ -> Nothing
        Source.OfToken _ -> Nothing
        Source.OfEmblem _ -> Nothing
        Source.OfSpellCopy _ -> Nothing
        Source.OfCardCopy _ -> Nothing
  pure (AbilityView.MkAbilityView (Game.abilitySourceOf oid gs >>= known) stacked)
  where
    known source = case Game.zoneOf source gs of
      Just zone | Game.isHiddenZone zone && Visibility.sees pid source gs /= Visibility.Type.Visible -> Nothing
      _ -> Just source
