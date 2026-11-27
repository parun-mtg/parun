-- | CR 901, the Planechase variant: the planar deck, CR 103.7's starting plane,
-- CR 701.31's planeswalk, and CR 901.9's planar die with the two things it can
-- set off -- CR 311.7's chaos and CR 901.8's planeswalking ability.
--
-- No GameSettings field for the variant itself, Parun.Engine.Vanguard's
-- posture: every rule here is stated of a player's planar deck or a plane card,
-- so having a planar deck is the whole of being in the variant (isPlanechase).
-- CR 901.15's single planar deck option is the one field, an option within it
-- (GameSettings.singlePlanarDeck).
module Parun.Engine.Planechase where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Engine.Cost as Cost
import qualified Parun.Engine.Decide as Decide
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Expiry as Expiry
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.GrandMelee as GrandMelee
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.MoveDuration as MoveDuration
import qualified Parun.Engine.Plane as Plane
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Turn as Turn
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.Bookkeeping as Bookkeeping
import Parun.Types.Card (Card)
import Parun.Types.Cost (Cost)
import qualified Parun.Types.Cost as Cost.Type
import qualified Parun.Types.Effect as Effect
import Parun.Types.Game (Game)
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.GrantedAbility as GrantedAbility
import qualified Parun.Types.InherentTriggerSource as InherentTriggerSource
import Parun.Types.Keyword (Keyword)
import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.LoggedEvent as LoggedEvent
import qualified Parun.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.PaymentSubject as PaymentSubject
import qualified Parun.Types.PendingTrigger as PendingTrigger
import qualified Parun.Types.PlanarDieFace as PlanarDieFace
import qualified Parun.Types.PlanarDieRolled as PlanarDieRolled
import qualified Parun.Types.PlanarDieTrigger as PlanarDieTrigger
import qualified Parun.Types.Planeswalk as Planeswalk
import qualified Parun.Types.PlaneswalkAway as PlaneswalkAway
import qualified Parun.Types.PlaneswalkCause as PlaneswalkCause
import qualified Parun.Types.PlaneswalkedFrom as PlaneswalkedFrom
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.ProposedEvent as ProposedEvent
import qualified Parun.Types.Settle as Settle
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.Source as Source
import qualified Parun.Types.Throw as Throw
import qualified Parun.Types.ThrowKind as ThrowKind
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerSource as TriggerSource
import Parun.Types.TriggeredAbility (TriggeredAbility)
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Parun.Types.TurnMarker as TurnMarker
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.TurnedUpOffDeck as TurnedUpOffDeck

-- | CR 901.3: this player's planar deck, top first; under CR 901.15c, the
-- communal one.
deckOf :: PlayerId -> GameState -> [ObjectId]
deckOf pid gs = Game.sideDeckOf SideDeck.Planar (Plane.planarDeckHolder pid gs) gs

-- | CR 901.15c: under the single planar deck option, every planar card any
-- seat holds, gathered into one deck under the lowest seat keyed, in seat order.
-- Nothing otherwise. For a game rebuilt from its cards, which regroups them by
-- owner (Parun.Engine.Setup's restart and subgame return).
gather :: GameState -> GameState
gather gs
  | GameSettings.singlePlanarDeck (MatchScope.settings (GameState.match gs)) =
      Game.overSideDecks SideDeck.Planar (\decks -> maybe decks (\(holder, _) -> Map.singleton holder (Foldable.fold decks)) (Map.lookupMin decks)) gs
  | otherwise = gs

-- | CR 901.15b: the cards of the communal planar deck, which its planar
-- controller owns, and none outside the single planar deck option. CR 800.4p
-- hands the designation on before a planar controller leaves, so no departing
-- player owns one (Parun.Engine.Departure.objectsLeaveWith).
-- Parun.PlanechaseSpec's "CR 901.15b the communal planar deck stays when the
-- player who brought it leaves" proves it.
communalDeck :: GameState -> Set.Set ObjectId
communalDeck gs
  | GameSettings.singlePlanarDeck (MatchScope.settings (GameState.match gs)) = foldMap (Set.fromList . Foldable.toList) (Game.sideDecksOf SideDeck.Planar gs)
  | otherwise = Set.empty

-- | CR 901.1: is this a Planechase game? Some player brought a planar deck.
isPlanechase :: GameState -> Bool
isPlanechase gs = not (Map.null (Game.sideDecksOf SideDeck.Planar gs))

-- | CR 901.4: the face-up plane and phenomenon cards.
faceUp :: GameState -> [ObjectId]
faceUp gs = filter (`Plane.isPlanarCard` gs) (Set.toAscList (GameState.command gs))

-- | CR 701.31a: may this player planeswalk? Only in a Planechase game, and only
-- a planar controller (Plane.planarControllers). So the planeswalking ability
-- of a Two-Headed Giant teammate who is not the primary player does nothing
-- (CR 901.8, 901.12b):
-- data/scenarios/planechase/cr-901-12b-the-active-team-s-primary-player-controls-the-plane.json
-- proves it.
canPlaneswalk :: PlayerId -> GameState -> Bool
canPlaneswalk pid gs = isPlanechase gs && List.elem pid (Plane.planarControllers gs)

-- | CR 901.14a: the Grand Melee turn marker a plane this player turns face up
-- comes under: the marker of the turn being worked if its planar control is
-- theirs, and otherwise the lowest-numbered one whose is. Nothing outside
-- Grand Melee.
--
-- Implementation-defined: CR 901.6's heir of a departed holder can carry two
-- markers' planar control, and the CR does not say under which their plane
-- comes.
markerOf :: PlayerId -> GameState -> Maybe Natural
markerOf pid gs =
  let theirs = [m | m <- GameState.turnMarkers gs, Plane.markerController m gs == Just pid]
      worked = [m | m <- theirs, Just (TurnMarker.number m) == Turn.workedMarker gs]
   in if Plane.isGrandMelee gs then fmap TurnMarker.number (Maybe.listToMaybe (worked <> theirs)) else Nothing

-- | CR 901.6: the controller of this face-up plane or phenomenon card on `gs`
-- (Plane.planarControllerOf).
controllerOfFaceUp :: ObjectId -> GameState -> Maybe PlayerId
controllerOfFaceUp oid gs = Game.lookupObject oid gs >>= (`Plane.planarControllerOf` gs)

-- | CR 701.31b: put each face-up plane and phenomenon card on the bottom of its
-- owner's planar deck, then turn the top card of this player's planar deck
-- face up -- or, given `to`, each of those cards still in that deck at once,
-- Spatial Merging's "Simultaneously planeswalk to both of them". Given
-- FromPhenomenaOnly, the face-up planes stay (Norn's Seedcore):
-- data/scenarios/planechase/norn-s-seedcore-planeswalks-to-a-revealed-plane-without-leaving-any.json
-- proves it. Nothing for a player CR 701.31a does not let planeswalk. Proposed first as WouldPlaneswalk
-- for this cause (CR 701.31c), so CR 614's replacements see it once.
--
-- In Grand Melee too every face-up card goes, whoever controls it (CR 701.31b,
-- 901.11c), and the plane turned up comes under this player's marker
-- (markerOf). Parun.PlanechaseSpec's "CR 901.11c a Grand Melee planeswalk
-- walks away from every face-up plane" proves it.
planeswalk :: PlaneswalkCause.PlaneswalkCause -> PlaneswalkAway.PlaneswalkAway -> Maybe [ObjectId] -> PlayerId -> Game ()
planeswalk cause from to pid = proposed cause pid $ do
  now <- State.get
  let leaves oid = case from of
        PlaneswalkAway.FromEveryCard -> True
        PlaneswalkAway.FromPhenomenaOnly -> maybe False Plane.isPhenomenonFace (Game.faceOf oid now)
      away = filter leaves (faceUp now)
      marker = markerOf pid now
      walked = walkedFrom now away
  bottomAll away
  case to of
    Nothing -> encounterTop marker pid walked (Set.fromList away)
    Just cards -> encounterAll marker pid cards walked (Set.fromList away)

-- CR 901.11b: does a planeswalk away from these face-up cards walk away from a
-- plane? Read before they go to the bottom.
walkedFrom :: GameState -> [ObjectId] -> PlaneswalkedFrom.PlaneswalkedFrom
walkedFrom gs away =
  let isPlane oid = maybe False (not . Plane.isPhenomenonFace) (Game.faceOf oid gs)
   in case filter isPlane away of
        [] -> PlaneswalkedFrom.AnyCard
        _ : _ -> PlaneswalkedFrom.APlane

-- CR 614.1a: propose this player's planeswalk for this cause, and perform it
-- unless a replacement took its place. Nothing for a player CR 701.31a does not
-- let planeswalk.
proposed :: PlaneswalkCause.PlaneswalkCause -> PlayerId -> Game () -> Game ()
proposed cause pid perform = do
  gs <- State.get
  Monad.when (canPlaneswalk pid gs) $ do
    outcome <- Event.applyReplacements (ProposedEvent.WouldPlaneswalk pid cause)
    Monad.forM_ outcome (const perform)

-- | CR 103.7: the starting player turns up planar cards until a plane is face
-- up, putting each phenomenon on the bottom. No ability triggers (CR 901.5):
-- nothing here records an event. Bounded by the deck's size, so a deck of
-- phenomena alone ends with none face up rather than looping. In Grand Melee
-- the plane comes under the turn marker given (CR 901.14a).
setStartingPlane :: Maybe Natural -> PlayerId -> Game ()
setStartingPlane marker pid = do
  gs <- State.get
  let go :: Int -> Game ()
      go n = Monad.when (n > 0) $ do
        turned <- turnUpTop marker pid
        g <- State.get
        case turned of
          Just oid | maybe False Plane.isPhenomenonFace (Game.faceOf oid g) -> toBottom oid >> go (n - 1)
          _ -> pure ()
  go (length (deckOf pid gs))

-- | CR 103.7 / 901.14a: the starting planes. The starting player sets one, or
-- in Grand Melee each player starting with a turn marker does, in marker
-- order, under that marker. Nothing outside a Planechase game.
-- Parun.PlanechaseSpec's "CR 901.14a each player with a turn marker sets a
-- starting plane" proves it.
setStartingPlanes :: Game ()
setStartingPlanes = do
  gs <- State.get
  Monad.when (isPlanechase gs) $
    if Plane.isGrandMelee gs
      then Monad.forM_ (GameState.turnMarkers gs) (\m -> setStartingPlane (Just (TurnMarker.number m)) (TurnMarker.holder m))
      else setStartingPlane Nothing (TurnRegisters.activePlayer (GameState.turn gs))

-- | CR 901.10 / 901.10a / 901.14b, run as these players leave the game and
-- read against `before`, the board as it was: if a face-up plane or phenomenon
-- card they own left with them, its planar controller -- already CR 800.4p's
-- heir -- turns up the top card of their planar deck, and if a face-up plane
-- left, every planeswalking ability on the stack ceases to exist. Not a
-- state-based action. The turn-up is a planeswalk (CR 701.31c), proposed for
-- CR 614's replacements, with nothing to put on the bottom: the face-up card
-- has left the game. Parun.PlanechaseSpec's "CR 701.31c Susan Foreman arranges
-- the planeswalk a plane's departing owner causes" proves it.
--
-- A phenomenon leaving alone spares the ability, CR 901.10a naming a plane.
-- Unobserved: a phenomenon is face up beside a planeswalking ability only after
-- a planeswalk in response to one, and the one card in data/cards that
-- planeswalks, Path of the Enigma, is a sorcery (2026-10-15).
--
-- The planeswalk walks away from the cards that left and, by CR 901.11c, from
-- every plane still face up beside them. Parun.PlanechaseSpec's "CR 901.11c a
-- departing owner's planeswalk walks away from the plane left face up" proves
-- it.
--
-- In Grand Melee each card's own planar controller turns up one card, under
-- that card's marker. A departure that costs a turn marker (CR 901.14b) first
-- ends the departing players' planar control, nobody taking it over
-- (GrandMelee.lapse): the face-up cards they controlled that stay in the
-- command zone go to the bottom, and nobody replaces the ones they owned and
-- controlled. Then a face-up card whose marker is gone goes to the bottom too
-- (markerlessToBottom). Parun.PlanechaseSpec's "CR 901.14b a departing marker
-- holder's planes go to the bottom and nobody planeswalks" proves it.
playersLeft :: GameState -> [PlayerId] -> Game ()
playersLeft before pids = do
  let owned oid = maybe False (\obj -> List.elem (Object.owner obj) pids) (Game.lookupObject oid before)
      left = filter owned (faceUp before)
      isPlane oid = maybe False (not . Plane.isPhenomenonFace) (Game.faceOf oid before)
      ceases = GrandMelee.cost pids before > 0
      departing = maybe False (`List.elem` pids)
      lapsing = [TurnMarker.number m | ceases, m <- GameState.turnMarkers before, departing (Plane.markerController m before)]
      unheld gs n = maybe False (\m -> List.notElem (TurnMarker.holder m) (Game.stillPlaying gs)) (GrandMelee.markerNumbered n gs)
  -- CR 901.14b: "first ceases to be a planar controller". A marker already
  -- passed on (CR 807.4c) carries its new holder's.
  State.modify' (\gs -> List.foldl' (flip GrandMelee.lapse) gs (filter (unheld gs) lapsing))
  stayed <- State.gets faceUp
  let ceased = [oid | ceases, oid <- stayed, departing (controllerOfFaceUp oid before)]
  bottomAll ceased
  Monad.unless (null left) $ do
    Monad.when (any isPlane left) (State.modify' ceasePlaneswalking)
    gs <- State.get
    let walkers = ListUtils.nubOrd [(walker, Object.planarMarker obj) | oid <- left, not (ceases && departing (controllerOfFaceUp oid before)), Just obj <- [Game.lookupObject oid before], Just walker <- [Plane.planarControllerOf obj gs]]
        away = Set.fromList (left <> filter (\oid -> isPlane oid && List.notElem oid ceased) (faceUp before))
    Monad.forM_ walkers (\(walker, marker) -> proposed PlaneswalkCause.OwnerLeft walker (encounterTop marker walker (walkedFrom before (Set.toList away)) away))
  markerlessToBottom

-- | CR 807.4g: a face-up plane or phenomenon card whose Grand Melee turn marker
-- is gone goes to the bottom of its owner's planar deck, and nobody
-- planeswalks (docs/decisions.md, "Where a removed turn marker's planes go").
-- Nothing outside Grand Melee. Parun.PlanechaseSpec's "CR 807.4g a removed
-- turn marker's plane goes to the bottom" proves it.
markerlessToBottom :: Game ()
markerlessToBottom = do
  gs <- State.get
  let numbers = fmap TurnMarker.number (GameState.turnMarkers gs)
      gone oid = maybe False (maybe True (`List.notElem` numbers) . Object.planarMarker) (Game.lookupObject oid gs)
  Monad.when (Plane.isGrandMelee gs) (bottomAll (filter gone (faceUp gs)))

-- CR 901.10a: every planeswalking ability on the stack ceases to exist.
ceasePlaneswalking :: GameState -> GameState
ceasePlaneswalking gs = List.foldl' (flip Game.cease) gs (filter (`isPlaneswalkingAbility` gs) (Game.allStacks gs))

-- | CR 901.8: is this object a planeswalking ability? Read as CR 901.8's own
-- inherent ability, which is the rule's and not a card's.
isPlaneswalkingAbility :: ObjectId -> GameState -> Bool
isPlaneswalkingAbility oid gs = case fmap Object.source (Game.lookupObject oid gs) of
  Just (Source.OfInherentTrigger inherent) -> InherentTriggerSource.ability inherent == planeswalkingAbility
  _ -> False

-- CR 701.31b's second half and CR 901.10's turn-up: move the top card off the
-- planar deck and turn it face up, which is the planeswalk itself (CR 901.11).
-- It records the GameEvent.TurnedUpOffDeck that CR 312.5's "When you
-- encounter" and CR 701.31d's "When you planeswalk to" and "away from" trigger
-- on, naming the cards `away` held face up, and ends every effect lasting
-- "until a player planeswalks" (and, when `walked` says it walked away from a
-- plane, "away from a plane"), marking every CR 610.3 and 610.4 watch on it
-- (Parun.Engine.MoveDuration.markPlaneswalked). CR 103.7's starting plane is turned up by
-- turnUpTop alone, CR 901.5 letting nothing trigger.
encounterTop :: Maybe Natural -> PlayerId -> PlaneswalkedFrom.PlaneswalkedFrom -> Set.Set ObjectId -> Game ()
encounterTop marker pid walked away = do
  turned <- turnUpTop marker pid
  Monad.forM_ turned $ \oid -> do
    State.modify' MoveDuration.markPlaneswalked
    State.modify' (Event.recordEvent (GameEvent.TurnedUpOffDeck (TurnedUpOffDeck.MkTurnedUpOffDeck SideDeck.Planar pid oid away)))
    State.modify' (Expiry.sweep (Expiry.Planeswalked walked))

-- CR 901.11 for several cards at once: turn each of `cards` still in this
-- player's planar deck face up, then record a GameEvent.TurnedUpOffDeck for
-- each -- so CR 312.5's and CR 701.31d's "planeswalk to" trigger for every one
-- with all of them face up -- the first alone naming `away`, so CR 701.31d's
-- "away from" triggers once. Effects lasting "until a player planeswalks" end
-- once. Parun.PlanechaseSpec's "CR 701.31d Spatial Merging planeswalks to two
-- planes at once" proves it.
encounterAll :: Maybe Natural -> PlayerId -> [ObjectId] -> PlaneswalkedFrom.PlaneswalkedFrom -> Set.Set ObjectId -> Game ()
encounterAll marker pid cards walked away = do
  turned <- Monad.filterM (turnUp marker pid) cards
  Monad.unless (null turned) (State.modify' MoveDuration.markPlaneswalked)
  Monad.forM_ (zip turned (away : repeat Set.empty)) $ \(oid, from) ->
    State.modify' (Event.recordEvent (GameEvent.TurnedUpOffDeck (TurnedUpOffDeck.MkTurnedUpOffDeck SideDeck.Planar pid oid from)))
  Monad.unless (null turned) (State.modify' (Expiry.sweep (Expiry.Planeswalked walked)))

-- | CR 704.6f / 312.7: the face-up phenomena that make their planar controller
-- planeswalk, each the source of no triggered ability on the stack or among
-- `waiting`, the abilities triggered on events not yet scanned
-- (Parun.Engine.Archenemy.abandonedBySba's reason).
spentPhenomena :: [PendingTrigger.PendingTrigger] -> GameState -> [ObjectId]
spentPhenomena waiting gs =
  let phenomena = filter (\oid -> maybe False Plane.isPhenomenonFace (Game.faceOf oid gs)) (faceUp gs)
      onStack oid = any (fromSource oid) (Game.allStacks gs)
      fromSource oid stacked = case fmap Object.source (Game.lookupObject stacked gs) of
        Just (Source.OfTrigger triggered) -> TriggeredAbilitySource.source triggered == oid
        _ -> False
      pending oid = any (\p -> PendingTrigger.source p == TriggerSource.OfObject oid) waiting
   in filter (\oid -> not (onStack oid) && not (pending oid)) phenomena

-- CR 701.31b's second half: move the top card off the planar deck and turn it
-- face up, which is joining GameState.command. A fresh timestamp (CR
-- 613.7h), which its static abilities' effects share (CR 613.7a). In Grand
-- Melee it comes under the turn marker given (Object.planarMarker).
--
-- Off the communal deck, the card keeps the owner CR 901.15b gave it there:
-- `pid`, the planar controller at every caller. See docs/decisions.md, "Who
-- owns a face-up card off the communal planar deck".
turnUpTop :: Maybe Natural -> PlayerId -> Game (Maybe ObjectId)
turnUpTop marker pid = do
  gs <- State.get
  case deckOf pid gs of
    [] -> pure Nothing
    top : _ -> (\moved -> if moved then Just top else Nothing) <$> turnUp marker pid top

-- turnUpTop for a card anywhere in this player's planar deck (Spatial
-- Merging's revealed planes). False, and nothing done, for a card not in it.
turnUp :: Maybe Natural -> PlayerId -> ObjectId -> Game Bool
turnUp marker pid card = do
  gs <- State.get
  let holder = Plane.planarDeckHolder pid gs
      owned o = if GameSettings.singlePlanarDeck (MatchScope.settings (GameState.match gs)) then o {Object.owner = pid} else o
  if List.notElem card (deckOf pid gs)
    then pure False
    else do
      ts <- State.state Game.freshTimestamp
      State.modify' $ \g ->
        (Game.overSideDecks SideDeck.Planar (Map.insert holder (Seq.fromList (List.delete card (deckOf pid g)))) g)
          { GameState.command = Set.insert card (GameState.command g),
            GameState.objects = Map.adjust (\o -> owned o {Object.timestamp = ts, Object.planarMarker = marker}) card (GameState.objects g)
          }
      pure True

-- CR 701.31b's first half for every face-up card at once: each owner puts
-- theirs on the bottom in the order they choose (CR 401.4, and Spatial
-- Merging's ruling of 2012-06-01), asked at two or more, the answer read from
-- the bottom inward as Prompt.ArrangeLibraryArrivals says.
bottomAll :: [ObjectId] -> Game ()
bottomAll away = do
  gs <- State.get
  let owners = ListUtils.nubOrd (Maybe.mapMaybe (fmap Object.owner . (`Game.lookupObject` gs)) away)
      ownedBy pid = filter (\oid -> fmap Object.owner (Game.lookupObject oid gs) == Just pid) away
  Monad.forM_ owners $ \owner -> do
    let batch = ownedBy owner
    ordered <- case batch of
      _ : _ : _ -> do
        g <- State.get
        Game.permute batch <$> Game.choose (Prompt.ArrangeLibraryArrivals (Decide.deciderFor owner g) owner LibraryPosition.Bottom batch)
      _ -> pure batch
    Monad.mapM_ toBottom (reverse ordered)

-- CR 701.31b's first half: a face-up card goes to the bottom of its owner's
-- planar deck (CR 901.15c: the communal one) face down, and CR 311.6 / 312.6
-- make it a new object. Plane.phenomenonOwnerOf reads the old id's record.
-- Unobserved, as playersLeft's phenomenon gate is: it needs a planeswalk while
-- that ability waits.
toBottom :: ObjectId -> Game ()
toBottom oid = State.modify' $ \gs ->
  maybe gs (\obj -> Event.turnFaceDownToBottom SideDeck.Planar (Plane.planarDeckHolder (Object.owner obj) gs) oid gs) (Game.lookupObject oid gs)

-- | CR 901.9: how many times this player has taken the special action this
-- turn. A fold over the event log, which is exactly "this turn"
-- (Parun.Engine.Coin.flipsThisTurn's reason). Each action records one roll with
-- a result, and its rerolls none (`roll`), and no effect rolls the planar die
-- today, so the count is CR 901.9's "times they have previously taken this
-- action".
rollsThisTurn :: PlayerId -> GameState -> Natural
rollsThisTurn pid gs =
  let rolled logged = case LoggedEvent.event logged of
        GameEvent.PlanarDieRolled r -> PlanarDieRolled.roller r == pid && Maybe.isJust (PlanarDieRolled.face r)
        _ -> False
   in Natural.length (filter rolled (Foldable.toList (Turn.eventsThisTurn gs)))

-- | CR 901.9: {1} for each earlier roll this turn, so the first is free.
rollCost :: PlayerId -> GameState -> Cost Keyword
rollCost pid gs =
  let n = rollsThisTurn pid gs
   in Cost.Type.MkCost
        { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n | n > 0]),
          Cost.Type.components = []
        }

-- | CR 116.2i / 901.9: may this player roll the planar die now? The active
-- player, in a main phase of their turn with an empty stack, able to pay.
canRoll :: PlayerId -> GameState -> Bool
canRoll pid gs =
  isPlanechase gs
    && Turn.sorcerySpeedWindow pid gs
    && Cost.canPay PaymentSubject.ForNeither pid (Bookkeeping.nextObjectId (GameState.bookkeeping gs)) (rollCost pid gs) gs

-- | CR 901.3a: a six-sided die with one Planeswalker face, one chaos face and
-- four blank ones. Which number stands for which face is parun's; anything the
-- answerer returns outside 1 to 6 is blank.
faceOf :: Natural -> PlanarDieFace.PlanarDieFace
faceOf n = case n of
  5 -> PlanarDieFace.Chaos
  6 -> PlanarDieFace.Planeswalker
  _ -> PlanarDieFace.Blank

-- | CR 116.2i / 901.9: pay, then roll the planar die, through CR 706.2's
-- modifiers from other sources (Settle.throwRecorded, which records
-- PlayerAction.RollDice, CR 901.9d). The performer's Settle always carries a
-- thrower (Resolve.Effect.settleWith), so a road with none rolls nothing. The final face records
-- PlanarDieRolled, which CR 901.8's planeswalking ability triggers on, and for
-- the chaos symbol ChaosEnsued, which CR 311.7's chaos abilities trigger on;
-- each reroll recorded a PlanarDieRolled with no face first
-- (Resolve.Effect.throwSettled's `recordRerolls`).
-- A payment that fails restores the state, Parun.Engine.Companion.take's
-- posture, and nothing is rolled.
roll :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> Game ()
roll perform pid = do
  before <- State.get
  Monad.when (canRoll pid before) $ do
    noSource <- State.state Game.freshObjectId
    paid <- Cost.payAction perform before PaymentSubject.ForNeither 0 pid noSource (rollCost pid before)
    Monad.forM_ paid $ \_ -> do
      kept <- maybe (pure []) ($ Throw.MkThrow {Throw.thrower = pid, Throw.kind = ThrowKind.PlanarDie, Throw.units = Seq.singleton 1, Throw.ignored = 0, Throw.instead = False}) (Settle.throwRecorded (ManaAbilityPerformer.settle perform))
      -- Exactly one: nothing adds a die to the planar die's roll.
      Foldable.for_ (Maybe.listToMaybe kept) $ \rolled -> do
        let face = faceOf rolled
        chaos <- State.gets (PlayerEffect.isChaosRoll pid face)
        State.modify' (Event.recordEvent (GameEvent.PlanarDieRolled (PlanarDieRolled.MkPlanarDieRolled pid (Just face))))
        -- CR 901.9b: the chaos symbol makes chaos ensue, and so does a blank
        -- roll Chaotic Aether makes a chaos roll, which stays a blank roll
        -- (docs/decisions.md, "Whether Chaotic Aether's chaos roll is still a
        -- blank roll").
        Monad.when chaos (State.modify' (Event.recordEvent (GameEvent.ChaosEnsued Nothing)))

-- | CR 901.8: the planeswalking ability, paired with each player, for
-- Parun.Engine.Event.Trigger.inherentTriggers. Planechase games have it; it "has
-- no source and is controlled by the player whose planar die roll caused it to
-- trigger", which is each player holding it with themselves as its "you".
abilities :: GameState -> [(PlayerId, TriggeredAbility Card (GrantedAbility.GrantedAbility Card))]
abilities gs = if isPlanechase gs then fmap (\pid -> (pid, planeswalkingAbility)) (Map.keys (GameState.players gs)) else []

-- | CR 901.8's text: "Whenever you roll the Planeswalker symbol on the planar
-- die, planeswalk."
planeswalkingAbility :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
planeswalkingAbility =
  Mint.trigger (TriggerCondition.PlayerRollsPlanarDie (PlanarDieTrigger.MkPlanarDieTrigger PlayerRelation.You (Just PlanarDieFace.Planeswalker))) (Seq.singleton (Effect.Planeswalk (Planeswalk.MkPlaneswalk Nothing PlaneswalkAway.FromEveryCard)))
