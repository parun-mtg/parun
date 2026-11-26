{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Parun.Types.Program where

import Control.Monad ((>=>))

-- | An operational (free) monad over an instruction functor 'instr'.
-- 'Then i k' suspends on instruction 'i' and resumes with continuation 'k'.
data Program instr a where
  Return :: a -> Program instr a
  Then :: instr b -> (b -> Program instr a) -> Program instr a

instance Functor (Program instr) where
  fmap f program = case program of
    Return a -> Return (f a)
    Then i k -> Then i (fmap f . k)

instance Applicative (Program instr) where
  pure = Return
  pf <*> px = do
    f <- pf
    fmap f px

instance Monad (Program instr) where
  program >>= f = case program of
    Return a -> f a
    Then i k -> Then i (k >=> f)

prompt :: instr a -> Program instr a
prompt i = Then i Return

-- Rewrite every instruction a program suspends on, leaving the control flow
-- alone. A natural transformation of the instruction functor, so it cannot see
-- or change an answer -- which is what makes it safe to apply to a program that
-- is already half built.
--
-- The one caller is Engine.playSubgame, which uses it to name the game each
-- suspension came from (CR 730.1a): a subgame runs as a nested StateT over THIS
-- program, so its instructions pass outward through the parent's frame and can
-- be told there, from outside, which game raised them.
mapProgram :: (forall b. instr b -> instr2 b) -> Program instr a -> Program instr2 a
mapProgram f program = case program of
  Return a -> Return a
  Then i k -> Then (f i) (mapProgram f . k)

foldProgram :: (forall b. instr b -> b) -> Program instr a -> a
foldProgram answer program = case program of
  Return a -> a
  Then i k -> foldProgram answer (k (answer i))

foldProgramM :: (Monad m) => (forall b. instr b -> m b) -> Program instr a -> m a
foldProgramM answer program = case program of
  Return a -> pure a
  Then i k -> do
    b <- answer i
    foldProgramM answer (k b)

-- | 'foldProgramM', where an answer can be given up for an earlier one: each
-- instruction `keyOf` keys is kept, with the program as it stood suspended on
-- it, while only keyed instructions follow; an answer that `catching` gives up
-- with a key kept suspends on that key's instruction again, those kept after
-- it forgotten, and one with a key not kept goes to `lost`. Since only keyed
-- instructions were answered between, nothing is undone but what `catching`
-- restores of the base monad. RemoteSeatSpec's game of three on the clocks
-- is the proof.
foldProgramResuming :: (Monad m, Eq k) => (forall b. instr b -> Maybe k) -> (forall b. m b -> (k -> m b) -> m b) -> (forall b. k -> m b) -> (forall b. instr b -> m b) -> Program instr a -> m a
foldProgramResuming keyOf catching lost answer = go []
  where
    go kept program = case program of
      Return a -> pure a
      Then i k -> do
        answered <- catching (fmap (Right . k) (answer i)) (pure . Left)
        case answered of
          Right rest -> go (foldMap (\key -> (key, program) : kept) (keyOf i)) rest
          Left key -> case dropWhile ((/= key) . fst) kept of
            (_, again) : earlier -> go earlier again
            [] -> lost key
