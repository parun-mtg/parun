-- | CR 904, the Archenemy variant: the scheme deck, CR 701.32's set in motion
-- (CR 703.4e's turn-based action), CR 701.33's abandon, and CR 704.6e's
-- state-based action for schemes, which CR 205.4h's ongoing schemes are exempt
-- from.
--
-- No GameSettings field, Parun.Engine.Vanguard's posture: every rule here is
-- stated of the archenemy's scheme deck or a scheme card, so a player who
-- brought a scheme deck is an archenemy (isArchenemy). CR 904.12b's Supervillain
-- Rumble, where every player is one, is that same reading. CR 904.3's and
-- 904.13d's scheme deck construction is Parun.Legality's.
module Parun.Engine.Archenemy where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Parun.Engine.Event as Event
import qualified Parun.Engine.Event.Trigger as Trigger
import qualified Parun.Engine.Game as Game
import qualified Parun.Engine.PlayerEffect as PlayerEffect
import qualified Parun.Engine.Replacement as Replacement
import qualified Parun.Engine.Scheme as Scheme
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Types.AttackOption as AttackOption
import qualified Parun.Types.Deck as Deck
import Parun.Types.Game (Game)
import qualified Parun.Types.GameEvent as GameEvent
import qualified Parun.Types.GameSettings as GameSettings
import Parun.Types.GameState (GameState)
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.MatchScope as MatchScope
import qualified Parun.Types.Object as Object
import Parun.Types.ObjectId (ObjectId)
import qualified Parun.Types.PendingTrigger as PendingTrigger
import Parun.Types.PlayerId (PlayerId)
import qualified Parun.Types.PlayerInstruction as PlayerInstruction
import qualified Parun.Types.ProposedEvent as ProposedEvent
import qualified Parun.Types.SideDeck as SideDeck
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TeamId as TeamId
import qualified Parun.Types.Teams as Teams
import qualified Parun.Types.TriggerSource as TriggerSource
import qualified Parun.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Parun.Types.TurnedUpOffDeck as TurnedUpOffDeck

-- | CR 904.3: this player's scheme deck, top first.
deckOf :: PlayerId -> GameState -> [ObjectId]
deckOf = Game.sideDeckOf SideDeck.Scheme

-- | CR 904.2a / 904.12b: is this player an archenemy? They brought a scheme
-- deck.
isArchenemy :: PlayerId -> GameState -> Bool
isArchenemy pid gs = Map.member pid (Game.sideDecksOf SideDeck.Scheme gs)

-- | CR 904.2a: the one player of this matchup bringing a scheme deck, when
-- exactly one does. Nothing for CR 904.12's Supervillain Rumble, whose every
-- player is an archenemy and which keeps a random starting player (CR
-- 904.12c), nor for a game with no archenemy.
sole :: [(PlayerId, Deck.Deck)] -> Maybe PlayerId
sole matchup = case [pid | (pid, deck) <- matchup, not (Map.null (Map.findWithDefault Map.empty SideDeck.Scheme (Deck.sideDecks deck)))] of
  [pid] -> Just pid
  _ -> Nothing

-- | CR 904.2: an Archenemy game's settings -- two teams, this archenemy alone
-- on one (CR 904.2a) and every other seat on the other (CR 904.2b), with the
-- attack multiple players and shared team turns options (CR 805.1). No choice
-- is made: CR 904.13's Commander option keeps the same two teams.
setUp :: PlayerId -> GameState -> GameState
setUp archenemy gs =
  let teamOf pid = TeamId.MkTeamId (if pid == archenemy then 0 else 1)
   in gs
        { GameState.match =
            (GameState.match gs)
              { MatchScope.settings =
                  (MatchScope.settings (GameState.match gs))
                    { GameSettings.teams = Teams.MkTeams (Map.fromList [(pid, teamOf pid) | pid <- GameState.seating gs]),
                      GameSettings.sharedTeamTurns = True,
                      GameSettings.attackOption = Just AttackOption.MultiplePlayers
                    }
              }
        }

-- | CR 904.5: the archenemy starts at 40 life where every other player starts
-- at 20, so 20 more.
lifeBonus :: Integer
lifeBonus = 20

-- | CR 314.4: the face-up scheme cards.
faceUp :: GameState -> [ObjectId]
faceUp gs = filter (`Scheme.isScheme` gs) (Set.toAscList (GameState.command gs))

-- | CR 701.32 / 904.9: this archenemy sets a scheme in motion -- the one
-- named (My Laughter Echoes' "that scheme"), or the top card of their scheme
-- deck. Proposed first as WouldPerform, so a row can change how many (Plots
-- That Span Centuries), and CR 701.32c sets several one at a time, each
-- proposed in its turn. A rewritten instruction names no scheme, so each of
-- its schemes comes off the top of the deck (docs/decisions.md, "Plots That
-- Span Centuries replaces a named scheme's set in motion with three off the
-- deck" and "Two Plots That Span Centuries set five schemes in motion").
--
-- CR 101.2 / 614.17c: nothing happens, and nothing is replaced, while an effect
-- says schemes can't be set in motion (All in Good Time,
-- PlayerEffect.schemesCantBeSetInMotion).
setInMotion :: Maybe ObjectId -> PlayerId -> Game ()
setInMotion named pid = do
  gs <- State.get
  Monad.unless (PlayerEffect.schemesCantBeSetInMotion gs) $ do
    outcome <- Event.applyReplacements (ProposedEvent.WouldPerform PlayerInstruction.SetInMotion Nothing pid 1)
    Monad.forM_ (outcome >>= Replacement.asPerformed PlayerInstruction.SetInMotion) $ \(setter, times) ->
      if times == 1
        then State.gets (\g -> Foldable.asum [named, Maybe.listToMaybe (deckOf setter g)]) >>= Foldable.traverse_ (setOne setter)
        else Monad.replicateM_ (Natural.toIntSaturating times) (setInMotion Nothing setter)

-- CR 701.32b: move this scheme off the top of its deck if it is there and turn
-- it face up, which is joining GameState.command, with a fresh timestamp
-- (Parun.Engine.Planechase.turnUpTop's reason). The event is recorded whether
-- or not either action was performed, rule 701.32b's last sentence, so a
-- scheme already face up is set in motion all the same (My Laughter Echoes);
-- it is what CR 904.9's "When you set this scheme in motion" triggers on, and
-- the game is filed after it so a trigger reads the schemes face up then (CR
-- 701.32c).
setOne :: PlayerId -> ObjectId -> Game ()
setOne pid oid = do
  gs <- State.get
  Monad.when (Scheme.isScheme oid gs) $ do
    Monad.when (Maybe.listToMaybe (deckOf pid gs) == Just oid) $ do
      ts <- State.state Game.freshTimestamp
      State.modify' $ \g ->
        (Game.overSideDecks SideDeck.Scheme (Map.adjust (Seq.drop 1) pid) g)
          { GameState.command = Set.insert oid (GameState.command g),
            GameState.objects = Map.adjust (\o -> o {Object.timestamp = ts}) oid (GameState.objects g)
          }
    State.modify' (Trigger.fileBoards . Event.recordEvent (GameEvent.TurnedUpOffDeck (TurnedUpOffDeck.MkTurnedUpOffDeck SideDeck.Scheme pid oid Set.empty)))

-- | CR 701.33a: may this scheme be abandoned? Only a face-up ongoing one.
canAbandon :: ObjectId -> GameState -> Bool
canAbandon oid gs = Set.member oid (GameState.command gs) && Scheme.isScheme oid gs && Scheme.isOngoing oid gs

-- | CR 701.33b: turn this scheme face down and put it on the bottom of its
-- owner's scheme deck. Nothing for one CR 701.33a does not allow.
abandon :: ObjectId -> Game ()
abandon oid = do
  gs <- State.get
  Monad.when (canAbandon oid gs) (State.modify' (toBottom oid))

-- | CR 701.33b / 704.6e: a face-up scheme goes to the bottom of its owner's
-- scheme deck, face down, a new object (CR 400.9). data/scenarios/archenemy's
-- "CR 400.9 an abandoned scheme set in motion again is a new object" proves
-- it.
toBottom :: ObjectId -> GameState -> GameState
toBottom oid gs = maybe gs (\obj -> Event.turnFaceDownToBottom SideDeck.Scheme (Object.owner obj) oid gs) (Game.lookupObject oid gs)

-- | CR 704.6e / 314.6: the face-up non-ongoing schemes to put on the bottom of
-- their decks, when no triggered ability of any scheme is on the stack or
-- waiting to be put on it. CR 205.4h exempts an ongoing scheme.
--
-- "Waiting" is asked of the trigger gather itself, over the events this settle
-- has not scanned, Parun.Engine.Dungeon.finished's reason: this pass runs before
-- Engine.placePendingTriggers, so an ability triggered on such an event is in
-- no stack and no batch yet.
abandonedBySba :: GameState -> [ObjectId]
abandonedBySba gs =
  let spent = filter (\oid -> not (Scheme.isOngoing oid gs)) (faceUp gs)
      schemeOnStack = any fromScheme (Game.allStacks gs)
      fromScheme stacked = case fmap Object.source (Game.lookupObject stacked gs) of
        -- Through last-known information: an abandoned scheme's id names no
        -- object (CR 400.9), and its ability may still be on the stack.
        Just (Source.OfTrigger triggered) -> maybe False Scheme.isSchemeFace (Game.faceOfWithLastKnown (TriggeredAbilitySource.source triggered) gs)
        _ -> False
      waiting = any pendingFromScheme (Trigger.eventTriggers (Event.unscannedGrouped gs) gs)
      pendingFromScheme pending = case PendingTrigger.source pending of
        TriggerSource.OfObject oid -> Scheme.isScheme oid gs
        TriggerSource.Sourceless -> False
   in if null spent || schemeOnStack || waiting then [] else spent
