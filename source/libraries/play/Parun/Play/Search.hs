{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The search bot: flat Monte Carlo over a seat's priority and combat
-- decisions. Each
-- option is played out in worlds dealt consistently with what the seat has
-- seen, the random bot in every seat, and the option winning most is taken.
module Parun.Play.Search where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.Projection as Projection
import qualified Parun.Engine.Projection.View as Projection.View
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Registry as Registry
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Combat as Combat
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.Features as Features
import qualified Parun.Types.Game as Game.Type
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Program as Program
import qualified Parun.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Visibility as Visibility.Type
import qualified Parun.Types.Weights as Weights

-- | What an owner's unseen cards may be, given the viewer and the game: the
-- contents (Object.source and Object.identity) to deal onto that owner's
-- unseen objects. #9344 swaps it for an inferred deck.
type Prior = PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> [Contents]

-- | What a card object is, apart from where it sits: the card behind it and
-- its identity (CR 108.3).
type Contents = (Source.Source, Maybe CardIdentity.CardIdentity)

-- | The known deck list: an owner's unseen objects hold exactly their list
-- less every card the viewer has seen, so dealing their own contents anew is
-- dealing from that remainder. It assumes each list is known.
known :: Prior
known viewer gs owner =
  [ (Object.source object, Object.identity object)
  | oid <- unseen viewer gs owner,
    object <- Foldable.toList (Engine.Game.lookupObject oid gs)
  ]

-- | CR 400.2: the objects in `owner`'s hand and library `viewer` may not see,
-- in hand-then-library order.
unseen :: PlayerId.PlayerId -> GameState.GameState -> PlayerId.PlayerId -> [ObjectId.ObjectId]
unseen viewer gs owner =
  let hidden oid = Visibility.sees viewer oid gs /= Visibility.Type.Visible
   in filter hidden (Foldable.toList (Map.findWithDefault mempty owner (GameState.hand gs)) <> Foldable.toList (Map.findWithDefault mempty owner (GameState.library gs)))

-- | A world `viewer` cannot tell from `gs`: each owner's unseen objects dealt
-- the prior's contents, shuffled, one each, every id staying where it is. Following
-- Magarena's showRandomizedHiddenCards (docs/prior-art-lessons.md, 10.3).
-- SearchSpec proves the viewer's view is unchanged.
sample :: (Monad m) => Prior -> PlayerId.PlayerId -> GameState.GameState -> State.StateT Generator.Type.Generator m GameState.GameState
sample prior viewer gs =
  Monad.foldM
    ( \world owner -> do
        dealt <- Generator.shuffle (prior viewer gs owner)
        pure world {GameState.objects = Foldable.foldl' (\objects (oid, contents) -> Map.adjust (\target -> target {Object.source = fst contents, Object.identity = snd contents}) oid objects) (GameState.objects world) (zip (unseen viewer gs owner) dealt)}
    )
    gs
    (Map.keys (GameState.players gs))

-- | The search bot for its seat: each option of a decision it searches,
-- where they offer a choice ('searchable'), played out in
-- `SearchBudget.samples` fresh 'sample's, every option in each sample from
-- the same world and random stream, the best total taken, one of the best
-- at random on a tie; every other prompt answered by the random bot
-- (Game.bot). It searches priority decisions ('options') and the first
-- declaration of attackers ('attacks') and of blockers ('blocks'). It reads
-- the whole game only through 'sample', which deals every card its seat
-- cannot see now; a card it saw before is dealt anew (#9345).
--
-- Not implemented: searching targets, which the random bot answers (#9409).
search :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Prior -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
search registry budget weights prior = Answerer.MkAnswerer $ \asked ->
  let best :: Game.Type.Game Result.Result -> PlayerId.PlayerId -> NonEmpty.NonEmpty a -> (a -> Pick) -> Game.Play m a
      best restart pid choices pick = do
        rounds <- Monad.replicateM (Natural.toIntSaturating (SearchBudget.samples budget)) $ do
          world <- sample prior pid (Asked.game asked)
          shared <- State.get
          traverse (\option -> State.put shared >> rollout registry budget weights restart pid (pick option) world) choices
        let scored = NonEmpty.zip (foldr (NonEmpty.zipWith (+)) (0 <$ choices) rounds) choices
            top = maximum (fmap fst scored)
        maybe (pure (NonEmpty.head choices)) Generator.pick (NonEmpty.nonEmpty [option | (total, option) <- NonEmpty.toList scored, total == top])
   in case Asked.prompt asked of
        Prompt.ChooseAction decider pid menu
          | searchable asked decider pid (options menu),
            Just choices <- NonEmpty.nonEmpty (options menu) ->
              best Engine.resumePriority pid choices (acting pid)
        Prompt.DeclareAttackers decider pid offered
          | searchable asked decider pid (attacks offered),
            Just choices <- NonEmpty.nonEmpty (attacks offered) ->
              best Engine.resumeTurnBasedAction pid choices (attacking pid)
        Prompt.DeclareBlockers decider pid blockers attackers
          | firstBlocks pid (Asked.game asked) && searchable asked decider pid (blocks blockers attackers),
            Just choices <- NonEmpty.nonEmpty (blocks blockers attackers) ->
              best Engine.resumeTurnBasedAction pid choices (blocking pid)
        _ -> Answerer.unwrap (Game.bot registry) asked

-- | The answer a playout gives the first prompt it is asked that it fits:
-- the searched option, given to the decision restarted.
newtype Pick = MkPick (forall r. Prompt.Prompt r -> Maybe r)

-- | `pid`'s priority decision answered `action`.
acting :: PlayerId.PlayerId -> Action.Action -> Pick
acting pid action = MkPick $ \prompt -> case prompt of
  Prompt.ChooseAction _ holder _ | holder == pid -> Just action
  _ -> Nothing

-- | `pid`'s declaration of attackers answered `declared`.
attacking :: PlayerId.PlayerId -> [ObjectId.ObjectId] -> Pick
attacking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareAttackers _ declarer _ | declarer == pid -> Just declared
  _ -> Nothing

-- | `pid`'s declaration of blockers answered `declared`.
blocking :: PlayerId.PlayerId -> Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId) -> Pick
blocking pid declared = MkPick $ \prompt -> case prompt of
  Prompt.DeclareBlockers _ declarer _ _ | declarer == pid -> Just declared
  _ -> Nothing

-- | The actions of a priority menu worth searching: all but CR 605.3a's mana
-- abilities activated at priority, which a payment activates as it needs
-- (CR 601.2g), and life paid for mana at priority (Action.PayLifeForMana).
options :: [Action.Action] -> [Action.Action]
options = filter $ \action -> case action of
  Action.ActivateManaAbility {} -> False
  Action.PayLifeForMana {} -> False
  _ -> True

-- | The declarations of attackers searched, from the creatures offered (CR
-- 508.1a): every subset of up to 'widest' declarations, else none, all, each
-- creature alone and all but each. The engine judges each (CR 508.1c, 508.1d).
attacks :: [ObjectId.ObjectId] -> [[ObjectId.ObjectId]]
attacks offered
  | 2 ^ length offered <= widest = List.subsequences offered
  | otherwise = ListUtils.nubOrd ([[], offered] <> fmap pure offered <> [List.delete oid offered | oid <- offered])

-- | The declarations of blockers searched, from the creatures offered and
-- the attackers they may block (CR 509.1a): every one blocking at most one
-- attacker each, where there are up to 'widest', else none and each blocker
-- alone on each attacker. The engine judges each (CR 509.1b, 509.1c).
blocks :: [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> [Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId)]
blocks blockers attackers
  | (length attackers + 1) ^ length blockers <= widest = fmap (Map.fromList . Maybe.catMaybes) (traverse (\blocker -> Nothing : [Just (blocker, Set.singleton attacker) | attacker <- attackers]) blockers)
  | otherwise = Map.empty : [Map.singleton blocker (Set.singleton attacker) | blocker <- blockers, attacker <- attackers]

-- | The most declarations 'attacks' and 'blocks' search exhaustively.
widest :: Int
widest = 16

-- | Whether `pid` makes the first declaration of blockers, the only one
-- Engine.resumeTurnBasedAction restarts: the first defending player's, from
-- no piles (CR 509.1, Combat.declareBlockers).
firstBlocks :: PlayerId.PlayerId -> GameState.GameState -> Bool
firstBlocks pid gs =
  let combat = TurnRegisters.combat (GameState.turn gs)
   in fmap (Engine.Game.primaryOf gs) (Maybe.listToMaybe (Combat.defenders combat)) == Just pid && Maybe.isNothing (Combat.pileBlocks combat)

-- | Whether a decision is searched: a choice among two or more, the
-- player deciding for themselves (CR 724.1), in a step of the main game,
-- where Engine.resumePriority or Engine.resumeTurnBasedAction restarts it.
searchable :: Asked.Asked GameState.GameState r -> Decider.Decider -> PlayerId.PlayerId -> [a] -> Bool
searchable asked decider pid choices =
  let gs = Asked.game asked
   in length (take 2 choices) == 2
        && Decider.unwrap decider == pid
        && null (Asked.enclosing asked)
        && null (GameState.turnMarkers gs)
        && Map.null (GameState.parkedTurns gs)

-- | One playout for `pid` from a world at a decision, `restart` asking it
-- again and `pick` answering it, scored 'won' a win, minus it a loss, 0 a
-- draw; a playout cut off once it begins `SearchBudget.turns` turns after the
-- decision's, or after `SearchBudget.prompts` prompts, is scored by
-- 'evaluate' at the prompt it stopped at, short of a win or a loss.
rollout :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Weights.Weights -> Game.Type.Game Result.Result -> PlayerId.PlayerId -> Pick -> GameState.GameState -> Game.Play m Integer
rollout registry budget weights restart pid (MkPick pick) world =
  let random = Game.consulting registry (Map.fromList [(seat, Game.bot registry) | seat <- Map.keys (GameState.players world)])
      final = TurnRegisters.turnNumber (GameState.turn world) + SearchBudget.turns budget
      go :: Natural -> Bool -> Program.Program (Asked.Asked GameState.GameState) (Result.Result, GameState.GameState) -> Game.Play m Integer
      go left pending program = case program of
        Program.Return (result, _) -> pure (score pid result)
        Program.Then asked k
          | left == 0 || TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) > final -> pure (max (1 - won) (min (won - 1) (evaluate weights pid (Asked.game asked))))
          | pending, Just answer <- pick (Asked.prompt asked) -> go (left - 1) False (k answer)
          | otherwise -> Answerer.unwrap random asked >>= go (left - 1) pending . k
   in go (SearchBudget.prompts budget) True (State.runStateT restart world)

-- | A result for `pid`.
score :: PlayerId.PlayerId -> Result.Result -> Integer
score pid result = case result of
  Result.Won winner | winner == pid -> won
  Result.Drawn -> 0
  _ -> negate won

-- | A win's score, above any cut-off playout's.
won :: Integer
won = 1000

-- | A position for `pid`: their 'features' weighted by `Weights.ours`, less
-- the highest of every other player's weighted by `Weights.theirs`.
evaluate :: Weights.Weights -> PlayerId.PlayerId -> GameState.GameState -> Integer
evaluate weights pid gs =
  let board = Projection.projectAll gs
      grants = Projection.View.controlGrants gs
      weighed side player = dot (side weights) (features board grants player gs)
   in weighed Weights.ours pid - maybe 0 maximum (NonEmpty.nonEmpty [weighed Weights.theirs other | other <- Map.keys (GameState.players gs), other /= pid])

-- | The weighted sum of a player's features.
dot :: Features.Features -> Features.Features -> Integer
dot weight count =
  sum [field weight * field count | field <- [Features.life, Features.hand, Features.power, Features.toughness, Features.lands, Features.permanents]]

-- | `pid`'s generic features, from the board projected and the control
-- grants of `gs`: what every player may see or count (CR 400.2, 402.3), and
-- no effect's identity.
features :: Map.Map ObjectId.ObjectId ProjectedCharacteristics.ProjectedCharacteristics -> [Projection.View.ControlGrant] -> PlayerId.PlayerId -> GameState.GameState -> Features.Features
features board grants pid gs =
  let controlled = [characteristics | (oid, characteristics) <- Map.toList board, Projection.View.controllerOfGiven grants oid gs == Just pid]
      typed cardType = filter (Set.member cardType . ProjectedCharacteristics.cardTypes) controlled
      total stat = sum (fmap (max 0 . Maybe.fromMaybe 0 . stat) (typed CardType.Creature))
   in Features.MkFeatures
        { Features.life = maybe 0 Player.life (Map.lookup pid (GameState.players gs)),
          Features.hand = toInteger (length (Map.findWithDefault mempty pid (GameState.hand gs))),
          Features.power = total ProjectedCharacteristics.power,
          Features.toughness = total ProjectedCharacteristics.toughness,
          Features.lands = toInteger (length (typed CardType.Land)),
          Features.permanents = toInteger (length controlled)
        }
