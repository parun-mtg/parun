{-# LANGUAGE GADTs #-}

-- Covers Parun.Play.Search and Parun.Engine.Engine.resumePriority: a sampled
-- world, a restarted priority decision, and playouts of one decision.
module Parun.SearchSpec where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
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
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Program as Program
import qualified Parun.Types.Prompt as Prompt
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Source as Source
import qualified Parun.Types.TurnRegisters as TurnRegisters

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
        playout option n = either (Spec.assertFailure s . Text.unpack) pure =<< Except.runExceptT (State.evalStateT (Search.rollout (S.fixtureRegistry []) budget viewer option gs) (S.seeded n))
    case Search.options menu of
      one : two : _ -> do
        first <- playout one 3
        _ <- playout two 3
        again <- playout one 3
        Spec.assertEqWith s "the first option's playout again" again first
      _ -> Spec.assertFailure s "a decision offers two options"
  where
    viewer = S.alice

-- Seat 1's first priority decision with a choice from turn 3 on, in a seeded
-- game of the bears decks, the random bot in both seats: the game as asked
-- and the menu offered.
decision :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, [Action.Action])
decision s registry = do
  decks <- fmap snd <$> BotSpec.bears s registry
  (_, found) <- State.runStateT (Game.run lifted Nothing Limits.standard [] (Map.fromList [(Command.idOf 1, capture), (Command.idOf 2, Game.bot lifted)]) (S.seeded 5) Map.empty (Command.seated 1 decks)) Nothing
  maybe (Spec.assertFailure s "no decision from turn 3 on") pure found
  where
    lifted :: (Monad m) => Registry.Registry (State.StateT (Maybe (GameState.GameState, [Action.Action])) m)
    lifted = Registry.hoist Trans.lift (S.fixtureRegistry [])
    capture :: (Monad m) => Answerer.Answerer (Asked.Asked GameState.GameState) (Game.Play (State.StateT (Maybe (GameState.GameState, [Action.Action])) m))
    capture = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
      Prompt.ChooseAction decider pid menu
        | Search.searchable asked decider pid (Search.options menu) && TurnRegisters.turnNumber (GameState.turn (Asked.game asked)) >= 3 -> do
            Game.underneath (State.put (Just (Asked.game asked, menu)))
            Game.stop (Text.pack "found")
      _ -> Answerer.unwrap (Game.bot lifted) asked

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
