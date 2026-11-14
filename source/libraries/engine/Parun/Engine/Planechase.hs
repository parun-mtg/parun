-- | CR 901, the Planechase variant: the planar deck, CR 103.7's starting plane,
-- CR 701.31's planeswalk, and CR 901.9's planar die with the two things it can
-- set off -- CR 311.7's chaos and CR 901.8's planeswalking ability.
--
-- No GameSettings field for the variant itself, Parun.Engine.Vanguard's
-- posture: every rule here is stated of a player's planar deck or a plane card,
-- so having a planar deck is the whole of being in the variant (isPlanechase).
-- CR 901.15's single planar deck option is the one field, an option within it
-- (GameSettings.singlePlanarDeck).
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 901.12's Two-Headed Giant and CR 901.14's Grand Melee options (#4465).
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
import qualified Parun.Engine.Mint as Mint
import qualified Parun.Engine.MoveDuration as MoveDuration
import qualified Parun.Engine.Plane as Plane
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
import qualified Parun.Types.PlaneswalkCause as PlaneswalkCause
import qualified Parun.Types.PlayerActed as PlayerActed
import qualified Parun.Types.PlayerAction as PlayerAction
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.ProposedEvent as ProposedEvent
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TriggerCondition as TriggerCondition
import qualified Parun.Types.TriggerSource as TriggerSource
import Parun.Types.TriggeredAbility (TriggeredAbility)
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
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
-- the planar controller.
canPlaneswalk :: PlayerId -> GameState -> Bool
canPlaneswalk pid gs = isPlanechase gs && pid == Plane.planarController gs

-- | CR 701.31b: put each face-up plane and phenomenon card on the bottom of its
-- owner's planar deck, then turn the top card of this player's planar deck
-- face up -- or, given `to`, each of those cards still in that deck at once,
-- Spatial Merging's "Simultaneously planeswalk to both of them". Nothing for a
-- player CR 701.31a does not let planeswalk. Proposed first as WouldPlaneswalk
-- for this cause (CR 701.31c), so CR 614's replacements see it once.
planeswalk :: PlaneswalkCause.PlaneswalkCause -> Maybe [ObjectId] -> PlayerId -> Game ()
planeswalk cause to pid = proposed cause pid $ do
  now <- State.get
  let away = faceUp now
  bottomAll away
  case to of
    Nothing -> encounterTop pid (Set.fromList away)
    Just cards -> encounterAll pid cards (Set.fromList away)

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
-- phenomena alone ends with none face up rather than looping.
setStartingPlane :: PlayerId -> Game ()
setStartingPlane pid = do
  gs <- State.get
  let go :: Int -> Game ()
      go n = Monad.when (n > 0) $ do
        turned <- turnUpTop pid
        g <- State.get
        case turned of
          Just oid | maybe False Plane.isPhenomenonFace (Game.faceOf oid g) -> toBottom oid >> go (n - 1)
          _ -> pure ()
  go (length (deckOf pid gs))

-- | CR 901.10 / 901.10a, run as these players leave the game and read against
-- `before`, the board as it was: if a face-up plane or phenomenon card they own
-- left with them, the planar controller -- already CR 800.4p's heir -- turns up
-- the top card of their planar deck, and if a face-up plane left, every
-- planeswalking ability on the stack ceases to exist. Not a state-based action.
-- The turn-up is a planeswalk (CR 701.31c), proposed for CR 614's replacements,
-- with nothing to put on the bottom: the face-up card has left the game.
-- Parun.PlanechaseSpec's "CR 701.31c Susan Foreman arranges the planeswalk a
-- plane's departing owner causes" proves it.
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
ownersLeft :: GameState -> [PlayerId] -> Game ()
ownersLeft before pids = do
  let owned oid = maybe False (\obj -> List.elem (Object.owner obj) pids) (Game.lookupObject oid before)
      left = filter owned (faceUp before)
      isPlane oid = maybe False (not . Plane.isPhenomenonFace) (Game.faceOf oid before)
  Monad.unless (null left) $ do
    Monad.when (any isPlane left) (State.modify' ceasePlaneswalking)
    gs <- State.get
    let walker = Plane.planarController gs
        away = Set.fromList (left <> filter isPlane (faceUp before))
    proposed PlaneswalkCause.OwnerLeft walker (encounterTop walker away)

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
-- "until a player planeswalks", marking every CR 610.3 and 610.4 watch on it
-- (Parun.Engine.MoveDuration.markPlaneswalked). CR 103.7's starting plane is turned up by
-- turnUpTop alone, CR 901.5 letting nothing trigger.
encounterTop :: PlayerId -> Set.Set ObjectId -> Game ()
encounterTop pid away = do
  turned <- turnUpTop pid
  Monad.forM_ turned $ \oid -> do
    State.modify' MoveDuration.markPlaneswalked
    State.modify' (Event.recordEvent (GameEvent.TurnedUpOffDeck (TurnedUpOffDeck.MkTurnedUpOffDeck SideDeck.Planar pid oid away)))
    State.modify' (Expiry.sweep Expiry.Planeswalked)

-- CR 901.11 for several cards at once: turn each of `cards` still in this
-- player's planar deck face up, then record a GameEvent.TurnedUpOffDeck for
-- each -- so CR 312.5's and CR 701.31d's "planeswalk to" trigger for every one
-- with all of them face up -- the first alone naming `away`, so CR 701.31d's
-- "away from" triggers once. Effects lasting "until a player planeswalks" end
-- once. Parun.PlanechaseSpec's "CR 701.31d Spatial Merging planeswalks to two
-- planes at once" proves it.
encounterAll :: PlayerId -> [ObjectId] -> Set.Set ObjectId -> Game ()
encounterAll pid cards away = do
  turned <- Monad.filterM (turnUp pid) cards
  Monad.unless (null turned) (State.modify' MoveDuration.markPlaneswalked)
  Monad.forM_ (zip turned (away : repeat Set.empty)) $ \(oid, from) ->
    State.modify' (Event.recordEvent (GameEvent.TurnedUpOffDeck (TurnedUpOffDeck.MkTurnedUpOffDeck SideDeck.Planar pid oid from)))
  Monad.unless (null turned) (State.modify' (Expiry.sweep Expiry.Planeswalked))

-- | CR 704.6f / 312.7: the face-up phenomena that make the planar controller
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
-- 613.7h), which its static abilities' effects share (CR 613.7a).
--
-- Off the communal deck, the card keeps the owner CR 901.15b gave it there:
-- `pid`, the planar controller at every caller. See docs/decisions.md, "Who
-- owns a face-up card off the communal planar deck".
turnUpTop :: PlayerId -> Game (Maybe ObjectId)
turnUpTop pid = do
  gs <- State.get
  case deckOf pid gs of
    [] -> pure Nothing
    top : _ -> (\moved -> if moved then Just top else Nothing) <$> turnUp pid top

-- turnUpTop for a card anywhere in this player's planar deck (Spatial
-- Merging's revealed planes). False, and nothing done, for a card not in it.
turnUp :: PlayerId -> ObjectId -> Game Bool
turnUp pid card = do
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
            GameState.objects = Map.adjust (\o -> owned o {Object.timestamp = ts}) card (GameState.objects g)
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
-- Unobserved, as ownersLeft's phenomenon gate is: it needs a planeswalk while
-- that ability waits.
toBottom :: ObjectId -> Game ()
toBottom oid = State.modify' $ \gs ->
  maybe gs (\obj -> Event.turnFaceDownToBottom SideDeck.Planar (Plane.planarDeckHolder (Object.owner obj) gs) oid gs) (Game.lookupObject oid gs)

-- | CR 901.9: how many times this player has rolled the planar die this turn.
-- A fold over the event log, which is exactly "this turn"
-- (Parun.Engine.Coin.flipsThisTurn's reason). Every roll is the special action's
-- today, so the count is CR 901.9's "times they have previously taken this
-- action".
rollsThisTurn :: PlayerId -> GameState -> Natural
rollsThisTurn pid gs =
  let rolled logged = case LoggedEvent.event logged of
        GameEvent.PlanarDieRolled r -> PlanarDieRolled.roller r == pid
        _ -> False
   in Natural.length (filter rolled (Foldable.toList (TurnRegisters.events (GameState.turn gs))))

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

-- | CR 116.2i / 901.9: pay, then roll the planar die. The roll records
-- PlayerAction.RollDice (CR 901.9d) and PlanarDieRolled, which CR 901.8's
-- planeswalking ability triggers on, and for the chaos symbol ChaosEnsued, which
-- CR 311.7's chaos abilities trigger on.
-- A payment that fails restores the state, Parun.Engine.Companion.take's
-- posture, and nothing is rolled.
roll :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> Game ()
roll perform pid = do
  before <- State.get
  Monad.when (canRoll pid before) $ do
    noSource <- State.state Game.freshObjectId
    paid <- Cost.payAction perform before PaymentSubject.ForNeither 0 pid noSource (rollCost pid before)
    Monad.forM_ paid $ \_ -> do
      rolled <- Game.ask (Prompt.RollDie 6)
      State.modify' (Event.recordEvent (GameEvent.PlayerActed (PlayerActed.MkPlayerActed PlayerAction.RollDice pid)))
      State.modify' (Event.recordEvent (GameEvent.PlanarDieRolled (PlanarDieRolled.MkPlanarDieRolled pid (faceOf rolled))))
      -- CR 901.9b: the chaos symbol makes chaos ensue.
      Monad.when (faceOf rolled == PlanarDieFace.Chaos) (State.modify' (Event.recordEvent (GameEvent.ChaosEnsued Nothing)))

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
  Mint.trigger (TriggerCondition.PlayerRollsPlaneswalker PlayerRelation.You) (Seq.singleton (Effect.Planeswalk Nothing))
