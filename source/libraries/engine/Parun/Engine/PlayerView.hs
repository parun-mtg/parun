-- One seat's view of a game: built field by field from the state, never as a
-- redacted copy of it, so a field the state gains reaches no seat, in process
-- or over the line, until it is added here on purpose.
module Parun.Engine.PlayerView where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Commander as Commander
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Summoning as Summoning
import qualified Parun.Engine.Turn as Turn
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.AbilityView as AbilityView
import qualified Parun.Types.Acquisition as Acquisition
import qualified Parun.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Parun.Types.Bookkeeping as Bookkeeping
import qualified Parun.Types.Card as Card
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Chosen as Chosen
import qualified Parun.Types.Combat as Combat
import qualified Parun.Types.ComponentCard as ComponentCard
import qualified Parun.Types.ExilePlayPermission as ExilePlayPermission
import qualified Parun.Types.Face as Face
import qualified Parun.Types.Facing as Facing
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.InherentTriggerSource as InherentTriggerSource
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.MergeComponent as MergeComponent
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PlayerView as PlayerView
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.SeatView as SeatView
import qualified Parun.Types.ShownComponent as ShownComponent
import qualified Parun.Types.Source as Source
import qualified Parun.Types.StackedAbility as StackedAbility
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.TypeLine as TypeLine
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
      PlayerView.remaining = TurnRegisters.remaining (GameState.turn gs),
      PlayerView.extra = extraStep gs,
      PlayerView.active = TurnRegisters.activePlayer (GameState.turn gs),
      PlayerView.priority = TurnRegisters.priority (GameState.turn gs),
      PlayerView.seats = Maybe.mapMaybe (seatView pid gs) (Turn.turnOrder gs),
      PlayerView.battlefield = Maybe.mapMaybe (objectView pid gs) (Foldable.toList (GameState.battlefield gs)),
      PlayerView.phasedOut = Maybe.mapMaybe (objectView pid gs) (Map.keys (GameState.phasedOut gs)),
      PlayerView.command = Maybe.mapMaybe (objectView pid gs) (Foldable.toList (GameState.command gs)),
      PlayerView.stack = Maybe.mapMaybe (objectView pid gs) (TurnRegisters.stack (GameState.turn gs)),
      PlayerView.attackers = Combat.attackers (TurnRegisters.combat (GameState.turn gs)),
      PlayerView.blockers = Combat.blockers (TurnRegisters.combat (GameState.turn gs)),
      PlayerView.parkedStacks = fmap (Maybe.mapMaybe (objectView pid gs) . TurnRegisters.stack) (GameState.parkedTurns gs),
      PlayerView.witnessed = witnessedIn (Turn.eventsThisTurn gs),
      PlayerView.witnessedLastTurn = witnessedIn (Game.turnLog WhichTurn.LastTurn Nothing gs),
      PlayerView.monarch = GameState.monarch gs,
      PlayerView.initiative = GameState.initiative gs,
      PlayerView.daytime = GameState.daytime gs,
      PlayerView.result = GameState.result gs,
      PlayerView.undone = Map.lookup pid (Bookkeeping.undone (GameState.bookkeeping gs))
    }
  where
    witnessedIn :: Seq.Seq LoggedEvent.LoggedEvent -> [LoggedEvent.LoggedEvent]
    witnessedIn = filter (\logged -> Visibility.witnessed pid logged == Visibility.Type.Visible) . Foldable.toList

-- | CR 500.8 / 500.9: whether the step under way is an extra one: begun after
-- a step no earlier in the turn's order (Turn.turnRank) began this turn, the
-- current step's own beginning aside, so a CR 514.3a cleanup step after the
-- first is one too. Of two like phases the later shows as the extra one, the
-- schedule recording no more; a skipped step logs no beginning (CR 500.11), so
-- it makes nothing extra.
extraStep :: GameState.GameState -> Bool
extraStep gs =
  let current = TurnRegisters.phase (GameState.turn gs)
      rank = Turn.turnRank (TurnRegisters.phasesReversed (GameState.turn gs))
      begun = Turn.stepsBegun gs
      earlier = foldMap (\(before, latest) -> if latest == current then before else begun) (List.unsnoc begun)
   in any (\step -> rank step >= rank current) earlier

-- | CR 400.2: a hidden zone by its size and the members `pid` may see; a public
-- one in full. A library card whose id reached a seat is re-minted when
-- shuffled (Parun.Engine.Event.forgetShown), so no seat follows it through.
-- The draft notes and the commander damage go to every seat (CR 905.2b,
-- 903.10a).
seatView :: PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> Maybe SeatView.SeatView
seatView pid gs seat = do
  player <- Map.lookup seat (GameState.players gs)
  let members zone = Game.zoneMembers zone seat gs
      public zone = Maybe.mapMaybe (objectView pid gs) (members zone)
      hidden zone = Maybe.mapMaybe (objectView pid gs) (filter (\oid -> Visibility.sees pid oid gs == Visibility.Type.Visible) (members zone))
  pure
    SeatView.MkSeatView
      { SeatView.player = seat,
        SeatView.status = Player.status player,
        SeatView.life = Player.life player,
        SeatView.counters = Player.counters player,
        SeatView.handSize = Natural.length (members Zone.Hand),
        SeatView.hand = hidden Zone.Hand,
        SeatView.librarySize = Natural.length (members Zone.Library),
        SeatView.library = hidden Zone.Library,
        SeatView.graveyard = public Zone.Graveyard,
        SeatView.exile = public Zone.Exile,
        SeatView.manaPool = Map.findWithDefault (Mana.MkMana []) seat (GameState.manaPool gs),
        SeatView.ringTemptations = Player.ringTemptations player,
        SeatView.speed = Player.speed player,
        SeatView.designations = Player.designations player,
        SeatView.completedDungeons = Player.completedDungeons player,
        SeatView.draftNotes = Player.draftNotes player,
        SeatView.commanderDamage = Commander.commanderDamageTo seat gs
      }

-- | CR 110.5: an object's status for anyone; its characteristics (as
-- projected, CR 613, with CR 305.7's strip written in) for a seat that may see
-- them, which for a face-down permanent or spell is every seat (CR 708.2); the
-- card behind a face-down object for a seat that may look at it (CR 406.3,
-- 708.5), and beside whichever of the two the seat gets its characteristics
-- under its own static abilities alone; the cards merged under a merged
-- permanent (CR 731.2); the foretell cost it was given with it (CR
-- 702.143e); what an ability came from; what a stack
-- object targets, which CR 601.2c / 602.2b / 603.3d announce to every seat;
-- and what the object has chosen (CR 607.2d), for a seat that sees it, less a
-- secret choice to all but its chooser and whoever controls them (CR 724.4);
-- and a commander's tax, for every seat (CR 903.8).
objectView :: PlayerId.PlayerId -> GameState.GameState -> ObjectId.ObjectId -> Maybe ObjectView.ObjectView
objectView pid gs oid = do
  object <- Game.lookupObject oid gs
  let seen = Visibility.sees pid oid gs == Visibility.Type.Visible
      public = Facing.isFaceDown (Object.facing object) && (Game.zoneOf oid gs == Just Zone.Battlefield || Game.zoneOf oid gs == Just Zone.Stack)
      faceDown = Facing.isFaceDown (Object.facing object) || Object.exiledFaceDown object
      -- A Killer Among Us's ruling: the acquisitions whose choice was made in
      -- secret by a player this seat is not privy to.
      unprivy players = maybe False (\chooser -> Set.notMember pid (Decide.privyTo chooser gs)) (Map.lookup PlayerChoice.SecretChooser players)
      secrets = Map.keysSet (Map.filter unprivy (Object.chosenPlayers object))
      chosen :: Chosen.Chosen a -> Map.Map Acquisition.Acquisition a
      chosen field = if public || seen then Map.withoutKeys (Game.objectChoices field object) secrets else Map.empty
      shown = public || (seen && not faceDown)
      gathered = Projection.gather gs
      -- The parts of the object's own static abilities (CR 604.1): an effect
      -- this object's text applies to itself is what that text says.
      ownOf = filter (maybe False ((== oid) . fst) . Projection.gEffect)
      own = ownOf gathered
      -- The card a face-down object is, on the board that shows it.
      looked = Projection.shownBoard oid gs
      lookedGathered = Projection.gather looked
      lookedOwn = ownOf lookedGathered
      -- CR 731.2: the components under the topmost one.
      under = case Object.source object of
        Source.OfMerge components -> NonEmpty.tail components
        _ -> []
      -- CR 702.140e: a merged permanent's copiable values hold every
      -- component's abilities, which are its cards' printed text.
      isMerged = not (null under)
      -- A face-up component is public (CR 400.2); a face-down one is seen by
      -- whoever could look at the permanent were it face down (CR 708.5).
      mayLookAt component =
        not (Facing.isFaceDown (ShownComponent.facing component))
          || Visibility.sees pid oid gs {GameState.objects = Map.insert oid object {Object.facing = ShownComponent.facing component} (GameState.objects gs)} == Visibility.Type.Visible
      named component = if mayLookAt component then fmap Just (componentName gs component) else Just Nothing
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
          if shown
            then Just (Projection.withRulesTextStrip oid gs (Projection.projectFrom gathered oid gs))
            else Nothing,
        ObjectView.face =
          if seen && faceDown
            then Just (Projection.projectFrom lookedGathered oid looked)
            else Nothing,
        ObjectView.ownEffects =
          if seen && faceDown
            then if isMerged || not (null lookedOwn) then Just (Projection.projectFrom lookedOwn oid looked) else Nothing
            else if shown && (isMerged || not (null own)) then Just (Projection.projectFrom own oid gs) else Nothing,
        ObjectView.printedSubtypes =
          if seen && not faceDown
            then foldMap (TypeLine.subtypes . Face.typeLine) (Projection.View.spellFaceOf oid gs)
            else [],
        ObjectView.merged = Seq.fromList (Maybe.mapMaybe named under),
        ObjectView.foretellCostReduction =
          if seen
            then Object.foretellCostReduction object
            else Nothing,
        ObjectView.ability = abilityView pid gs oid,
        ObjectView.targets = Set.toList (Projection.View.targetsOfStackObject gs oid),
        ObjectView.summoningSick =
          Game.zoneOf oid gs == Just Zone.Battlefield
            && Set.member CardType.Creature (Projection.cardTypesOf oid gs)
            && not (Summoning.settledOrHasty (Maybe.fromMaybe (Object.owner object) (Projection.View.controllerOf oid gs)) oid gs),
        ObjectView.ringBearerFor = Object.ringBearerFor object,
        ObjectView.protector = Object.protector object,
        ObjectView.classLevel = Object.classLevel object,
        ObjectView.unlockedHalves = Object.unlockedHalves object,
        ObjectView.designations = Object.designations object,
        ObjectView.onAdventure = any ((== PlayPermissionOrigin.Adventure) . ExilePlayPermission.origin) (Object.playPermissions object),
        ObjectView.ventureRoom = Object.ventureRoom object,
        ObjectView.chosenColors = chosen Chosen.Colors,
        ObjectView.chosenColorless = chosen Chosen.Colorless,
        ObjectView.chosenSubtype = chosen Chosen.Subtype,
        ObjectView.chosenCardType = chosen Chosen.CardType,
        ObjectView.chosenNames = chosen Chosen.Names,
        ObjectView.chosenPlayers = if public || seen then Object.chosenPlayers object else Map.empty,
        ObjectView.chosenNumber = chosen Chosen.Number,
        ObjectView.chosenObjects = chosen Chosen.Objects,
        ObjectView.commanderTax = Commander.tax (Object.owner object) oid gs
      }

-- | CR 731.2: a component of a merged permanent by the face it shows: its
-- own (ShownComponent.face), a conjured duplicate's name (Object.duplicate),
-- or else its card's front face.
componentName :: GameState.GameState -> ShownComponent.ShownComponent -> Maybe CardName.CardName
componentName gs component = case ShownComponent.face component of
  Just face -> Just face
  Nothing -> case ShownComponent.component component of
    MergeComponent.OfCard card | Just duplicate <- ComponentCard.duplicate card -> Set.lookupMin (ProjectedCharacteristics.names duplicate)
    other -> fmap (Face.name . NonEmpty.head . Card.faces) (Game.cardOfPrinting (Game.printingOfComponent other) gs)

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
