{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The search bot: flat Monte Carlo over a seat's priority decisions. Each
-- option is played out in worlds dealt consistently with what the seat has
-- seen, the random bot in every seat, and the option winning most is taken.
module Parun.Play.Search where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import Numeric.Natural (Natural)
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.Visibility as Visibility
import qualified Parun.Extra.Natural as Natural
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Generator as Generator
import qualified Parun.Registry as Registry
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CardIdentity as CardIdentity
import qualified Parun.Types.Decider as Decider
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Program as Program
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.Result as Result
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Source as Source
import qualified Parun.Types.Visibility as Visibility.Type

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

-- | The search bot for its seat: each of a priority decision's 'options',
-- where they offer a choice ('searchable'), played out
-- `SearchBudget.samples` times, each in a fresh 'sample', the best total
-- taken, the earliest on a tie; every other prompt answered by the random bot
-- (Game.bot). It reads the whole game only through 'sample', which deals
-- every card its seat cannot see now; a card it saw before is dealt anew
-- (#9345).
--
-- Not implemented: searching attackers, blockers and targets, which the
-- random bot answers (#9388).
search :: (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> Prior -> Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play m)
search registry budget prior = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction decider pid menu
    | searchable asked decider pid (options menu) -> do
        scored <- traverse (\option -> fmap (\scores -> (sum scores, option)) (Monad.replicateM (Natural.toIntSaturating (SearchBudget.samples budget)) (sample prior pid (Asked.game asked) >>= rollout registry budget pid option))) (options menu)
        pure (snd (List.minimumBy (Ord.comparing (Ord.Down . fst)) scored))
  _ -> Answerer.unwrap (Game.bot registry) asked

-- | The actions of a priority menu worth searching: all but CR 605.3a's mana
-- abilities activated at priority, which a payment activates as it needs
-- (CR 601.2g), and life paid for mana at priority (Action.PayLifeForMana).
options :: [Action.Action] -> [Action.Action]
options = filter $ \action -> case action of
  Action.ActivateManaAbility {} -> False
  Action.PayLifeForMana {} -> False
  _ -> True

-- | Whether a priority decision is searched: a choice among two or more, the
-- player deciding for themselves (CR 724.1), in a step's own priority round
-- of the main game, where Engine.resumePriority restarts it.
searchable :: Asked.Asked GameState.GameState r -> Decider.Decider -> PlayerId.PlayerId -> [Action.Action] -> Bool
searchable asked decider pid menu =
  let gs = Asked.game asked
   in length (take 2 menu) == 2
        && Decider.unwrap decider == pid
        && null (Asked.enclosing asked)
        && null (GameState.turnMarkers gs)
        && Map.null (GameState.parkedTurns gs)

-- | One playout of `option` for `pid` from a world at their priority decision,
-- scored in halves: 2 a win, 1 a draw, 0 a loss; a playout cut off after
-- `SearchBudget.prompts` prompts is scored by 'standing'.
rollout :: forall m. (Monad m) => Registry.Registry m -> SearchBudget.SearchBudget -> PlayerId.PlayerId -> Action.Action -> GameState.GameState -> Game.Play m Natural
rollout registry budget pid option world =
  let random = Game.consulting registry (Map.fromList [(seat, Game.bot registry) | seat <- Map.keys (GameState.players world)])
      go :: Natural -> Bool -> Program.Program (Asked.Asked GameState.GameState) (Result.Result, GameState.GameState) -> Game.Play m Natural
      go left pending program = case program of
        Program.Return (result, _) -> pure (score pid result)
        Program.Then asked k
          | left == 0 -> pure (standing pid (Asked.game asked))
          | otherwise -> case Asked.prompt asked of
              Prompt.ChooseAction _ holder _
                | pending && holder == pid -> go (left - 1) False (k option)
              _ -> Answerer.unwrap random asked >>= go (left - 1) pending . k
   in go (SearchBudget.prompts budget) True (State.runStateT Engine.resumePriority world)

-- | A result for `pid`, in halves.
score :: PlayerId.PlayerId -> Result.Result -> Natural
score pid result = case result of
  Result.Won winner | winner == pid -> 2
  Result.Drawn -> 1
  _ -> 0

-- | A cut-off playout for `pid`, in halves: 2 if their life is above every
-- other player's, 0 if some other player's is above theirs, 1 otherwise. The
-- crudest score; #9342 replaces it.
standing :: PlayerId.PlayerId -> GameState.GameState -> Natural
standing pid gs =
  let lives = Map.map Player.life (GameState.players gs)
      own = Map.findWithDefault 0 pid lives
      others = Map.elems (Map.delete pid lives)
   in if all (< own) others then 2 else if any (> own) others then 0 else 1
