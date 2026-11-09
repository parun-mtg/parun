{-# LANGUAGE GADTs #-}

-- | Chance: every random prompt answered from the generator.
module Parun.Play.Chance where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Parun.Engine.Replay as Replay
import qualified Parun.Play.Generator as Generator
import qualified Parun.Types.Answerer as Answerer
import qualified Parun.Types.Asked as Asked
import qualified Parun.Types.CoinFace as CoinFace
import qualified Parun.Types.GameState as GameState
import qualified Parun.Types.Generator as Generator.Type
import qualified Parun.Types.Prompt as Prompt

-- | Each random prompt drawn from the generator (CR 701.24a's shuffle, CR
-- 706's die, CR 705's coin); every other prompt answered by
-- 'Replay.defaultAnswer'.
chance :: (Monad m) => Answerer.Answerer (Asked.Asked GameState.GameState) (State.StateT Generator.Type.Generator m)
chance = Answerer.MkAnswerer $ \asked -> case Asked.prompt asked of
  Prompt.Shuffle ids -> Generator.shuffle ids
  Prompt.RandomFirstPlayer order -> Generator.pick order
  Prompt.RandomObject candidates -> Generator.pick candidates
  Prompt.RandomStickerSheet slots -> Generator.pick slots
  Prompt.RandomPlayer candidates -> Generator.pick candidates
  Prompt.RandomCard candidates -> Generator.pick candidates
  Prompt.RollDie sides -> Generator.between 1 sides
  Prompt.RandomDepth depth -> Generator.between 1 depth
  Prompt.FlipCoin -> Generator.pick (CoinFace.Heads NonEmpty.:| [CoinFace.Tails])
  prompt -> pure (Replay.defaultAnswer prompt)
