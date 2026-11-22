{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Covers Parun.Play.Search, Parun.Engine.Engine.resumePriority and
-- Parun.Engine.Engine.resumeTurnBasedAction: a sampled world, a restarted
-- priority decision or combat declaration, and playouts of one decision.
module Parun.SearchSpec where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Parun.BotSpec as BotSpec
import qualified Parun.Engine.Engine as Engine
import qualified Parun.Engine.Game as Engine.Game
import qualified Parun.Engine.PlayerView as PlayerView
import qualified Parun.Play.Command as Command
import qualified Parun.Play.Game as Game
import qualified Parun.Play.Search as Search
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Support as S
import qualified Parun.Types.Action as Action
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.Concession as Concession
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Limits as Limits
import qualified Parun.Types.Object as Object
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.Player as Player
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Program as Program
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TapState as TapState
import qualified Parun.Types.TurnRegisters as TurnRegisters
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Search" $ do
  Spec.it s "CR 400.2 a sampled world looks the same to its viewer, its unseen cards dealt anew" $ do
    (gs, _) <- decision s registry
    worlds <- traverse (\n -> either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.sample Search.known viewer gs) (S.seeded n))) [1 .. 8 :: Word.Word32]
    Foldable.for_ worlds $ \world -> do
      Spec.assertEqWith s "the viewer's view" (PlayerView.viewFor viewer world) (PlayerView.viewFor viewer gs)
      Spec.assertEqWith s "the opponent's unseen cards, as a whole" (List.sort (sources world (Search.unseen viewer gs S.bob))) (List.sort (sources gs (Search.unseen viewer gs S.bob)))
    Spec.assertBool s (any (\world -> handOf world S.bob /= handOf gs S.bob) worlds) "some world deals the opponent another hand"

  Spec.it s "a restarted priority decision asks its holder again, in the same step, offering the same menu" $ do
    (gs, menu) <- decision s registry
    Spec.assertEqWith s "the step and the menu asked again" (asksAgain (State.runStateT Engine.resumePriority gs)) (Just (TurnRegisters.phase (GameState.turn gs), menu))

  Spec.it s "two playouts of one decision share no state" $ do
    (gs, menu) <- decision s registry
    let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 1, SearchBudget.prompts = 400}
        playout option n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget Engine.resumePriority viewer (Search.acting viewer option) gs) (S.seeded n))
    case Search.options menu of
      one : two : _ -> do
        first <- playout one 3
        _ <- playout two 3
        again <- playout one 3
        Spec.assertEqWith s "the first option's playout again" again first
      _ -> Spec.assertFailure s "a decision offers two options"

  Spec.it s "CR 508.1 a restarted declaration of attackers asks its declarer again, offering the same creatures" $ do
    (gs, offered) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, (pid, candidates))
      _ -> Nothing
    Spec.assertEqWith s "the declarer and the creatures asked again" (declaresAgain (State.runStateT Engine.resumeTurnBasedAction gs)) (Just (Left offered))

  Spec.it s "CR 509.1 a restarted declaration of blockers asks its declarer again, offering the same creatures" $ do
    (gs, offered) <- capturing s registry 3 "a declaration of blockers" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareBlockers decider pid blockers attackers | Search.firstBlocks pid (Asked.game asked) && Search.searchable asked decider pid (Search.blocks blockers attackers) -> Just (Asked.game asked, (pid, blockers, attackers))
      _ -> Nothing
    Spec.assertEqWith s "the declarer and the creatures asked again" (declaresAgain (State.runStateT Engine.resumeTurnBasedAction gs)) (Just (Right offered))

  Spec.it s "the search bot attacks an opponent at 1 life who has nothing untapped" $ do
    (_, asked) <- openAttack s registry
    declared <- traverse (searched s asked) [1 .. 4]
    Spec.assertBool s (not (any null declared)) "attacked in every seed"

  Spec.it s "parun play's bot seat is the search bot, attacking an opponent at 1 life who has nothing untapped" $ do
    (pid, asked) <- openAttack s registry
    let seat = PlayerId.unwrap pid + 1
        options = PlayOptions.none {PlayOptions.bots = Set.singleton seat}
        answered seed = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Command.answerer (S.fixtureRegistry []) (const (pure Nothing)) (pure Nothing) (const (pure ())) options seat) asked) (S.seeded seed))
    declared <- traverse answered [1 .. 8]
    Spec.assertBool s (not (any null declared)) "attacked in every seed"

  Spec.it s "the search bot blocks a lone attacker at 1 life" $ do
    (gs, (decider, pid, blockers, attackers)) <- capturing s registry 3 "a declaration of blockers against one attacker" $ \asked -> case Asked.prompt asked of
      Prompt.DeclareBlockers decider pid blockers attackers | length attackers == 1 && Search.firstBlocks pid (Asked.game asked) && Search.searchable asked decider pid (Search.blocks blockers attackers) -> Just (Asked.game asked, (decider, pid, blockers, attackers))
      _ -> Nothing
    declared <- traverse (searched s (Asked.MkAsked [] (atOneLife pid gs) (Prompt.DeclareBlockers decider pid blockers attackers))) [1 .. 4]
    Spec.assertBool s (all (\declaration -> all (\attacker -> any (Set.member attacker) (Map.elems declaration)) attackers) declared) "blocked in every seed"
  where
    viewer = S.alice

-- Seat 1's first priority decision with a choice from turn 3 on, in a seeded
-- game of the bears decks, the random bot in both seats: the game as asked
-- and the menu offered.
decision :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, [Action.Action])
decision s registry = capturing s registry 5 "a decision" $ \asked -> case Asked.prompt asked of
  Prompt.ChooseAction decider pid menu
    | pid == S.alice && Search.searchable asked decider pid (Search.options menu) -> Just (Asked.game asked, menu)
  _ -> Nothing

-- The first prompt from turn 3 on that `found` takes, in a game of the bears
-- decks from `seed`, the random bot in both seats answering everything else.
capturing :: forall m n x. (Monad m) => Spec.Spec m n -> Registry.Registry m -> Word.Word32 -> String -> (forall r. Asked.Asked GameState.GameState r -> Maybe (GameState.GameState, x)) -> m (GameState.GameState, x)
capturing s registry seed what found = do
  decks <- fmap snd <$> BotSpec.bears s registry
  (_, taken) <- State.runStateT (Game.run lifted Nothing Limits.standard [] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, capture)]) (S.seeded seed) Map.empty (Command.seated 1 decks)) Nothing
  maybe (Spec.assertFailure s ("no " <> what <> " from turn 3 on")) pure taken
  where
    lifted :: Registry.Registry (State.StateT (Maybe (GameState.GameState, x)) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    capture :: Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT (Maybe (GameState.GameState, x)) m))
    capture = Answerer.MkAnswerer $ \asked -> case found asked of
      Just taken
        | TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3 -> do
            Game.underneath (State.put (Just taken))
            Game.stop (Text.pack "found")
      _ -> Answerer.unwrap (Game.bot lifted) asked

-- A declaration of attackers from turn 3 on, every opponent of its declarer
-- at 1 life with every permanent tapped: the declarer and the prompt.
openAttack :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (PlayerId.PlayerId, Asked.Asked GameState.GameState [ObjectId.ObjectId])
openAttack s registry = do
  (gs, (decider, pid, candidates)) <- capturing s registry 3 "a declaration of attackers" $ \asked -> case Asked.prompt asked of
    Prompt.DeclareAttackers decider pid candidates | Search.searchable asked decider pid (Search.attacks candidates) -> Just (Asked.game asked, (decider, pid, candidates))
    _ -> Nothing
  let world = othersAtOneLife pid gs {GameState.objects = Map.map (\o -> if Object.owner o /= pid && Object.zone o == Zone.Battlefield then o {Object.tapped = TapState.Tapped} else o) (GameState.objects gs)}
  pure (pid, Asked.MkAsked [] world (Prompt.DeclareAttackers decider pid candidates))

-- The search bot's answer, at a budget of 2 samples cut off after 40 prompts,
-- from `seed`.
searched :: (Monad m) => Spec.Spec m n -> Asked.Asked GameState.GameState r -> Word.Word32 -> m r
searched s asked seed =
  let budget = SearchBudget.MkSearchBudget {SearchBudget.samples = 2, SearchBudget.prompts = 40}
   in either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Answerer.unwrap (Search.search (S.fixtureRegistry []) budget Search.known) asked) (S.seeded seed))

-- The game with every player but `pid` at 1 life.
othersAtOneLife :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
othersAtOneLife pid gs = gs {GameState.players = Map.mapWithKey (\p player -> if p == pid then player else player {Player.life = 1}) (GameState.players gs)}

-- The game with `pid` at 1 life.
atOneLife :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
atOneLife pid gs = gs {GameState.players = Map.adjust (\player -> player {Player.life = 1}) pid (GameState.players gs)}

-- The declarer and the creatures of the first combat declaration a program
-- asks; Nothing if it asks anything else first.
declaresAgain :: Program.Program (Asked.Asked GameState.GameState) a -> Maybe (Either (PlayerId.PlayerId, [ObjectId.ObjectId]) (PlayerId.PlayerId, [ObjectId.ObjectId], [ObjectId.ObjectId]))
declaresAgain program = case program of
  Program.Then asked _ -> case Asked.prompt asked of
    Prompt.DeclareAttackers _ pid candidates -> Just (Left (pid, candidates))
    Prompt.DeclareBlockers _ pid blockers attackers -> Just (Right (pid, blockers, attackers))
    _ -> Nothing
  Program.Return _ -> Nothing

-- The step and the menu of the first priority decision a program asks of seat
-- 1, once it has not conceded; Nothing if it asks anything else first.
asksAgain :: Program.Program (Asked.Asked GameState.GameState) a -> Maybe (Phase.Phase, [Action.Action])
asksAgain program = case program of
  Program.Then asked k -> case Asked.prompt asked of
    Prompt.Concede pid | pid == S.alice -> asksAgain (k Concession.Continues)
    Prompt.ChooseAction _ pid menu | pid == S.alice -> Just (TurnRegisters.phase (GameState.turn (Asked.game asked)), menu)
    _ -> Nothing
  Program.Return _ -> Nothing

-- The cards behind objects.
sources :: GameState.GameState -> [ObjectId.ObjectId] -> [Source.Source]
sources gs = foldMap (foldMap (pure . Object.source) . (`Engine.Game.lookupObject` gs))

-- A player's hand, the card behind each object in order.
handOf :: GameState.GameState -> PlayerId.PlayerId -> [Source.Source]
handOf gs pid = sources gs (Foldable.toList (Map.findWithDefault mempty pid (GameState.hand gs)))
