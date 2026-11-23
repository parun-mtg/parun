module Parun.Play.OpponentSpec where

import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.List as List
import qualified Numeric.Natural as Natural
import qualified Parun.Play.Opponent as Opponent
import qualified Parun.Spec as Spec
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.MatchOptions as MatchOptions
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.PlayOptions as PlayOptions
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Play.Opponent" $ do
  Spec.describe s "budgetAt" $ do
    Spec.it s "makes fewer mistakes at each level up, looking at no fewer playouts" $ do
      let levels = [minBound .. maxBound] :: [Difficulty.Difficulty]
          rising :: (Ord a) => [a] -> Bool
          rising xs = and (zipWith (<) xs (drop 1 xs))
          ordered base = do
            let budgets = fmap (`Opponent.budgetAt` base) levels
            Spec.assertBool s (rising (fmap (negate . toInteger . SearchBudget.mistakes) budgets)) ("fewer mistakes each level up: " <> show (fmap SearchBudget.mistakes budgets))
            Spec.assertEq s (fmap SearchBudget.samples budgets) (List.sort (fmap SearchBudget.samples budgets))
      ordered PlayOptions.budget
      ordered MatchOptions.budget

    Spec.it s "keeps how far a playout looks" $ do
      let base = PlayOptions.budget
          reach b = (SearchBudget.turns b, SearchBudget.prompts b, SearchBudget.candidates b, SearchBudget.playout b)
      Spec.assertEq s (fmap (reach . (`Opponent.budgetAt` base)) [minBound .. maxBound :: Difficulty.Difficulty]) (reach base <$ [minBound .. maxBound :: Difficulty.Difficulty])

    Spec.it s "plays the hardest as parun play's bot and parun match's search bot play unless told otherwise" $ do
      Spec.assertEq s (Opponent.budgetAt Difficulty.MkHard PlayOptions.budget) PlayOptions.budget
      Spec.assertEq s (Opponent.budgetAt Difficulty.MkHard MatchOptions.budget) MatchOptions.budget

    Spec.it s "sets every mistake rate a percent" $ do
      Spec.assertBool s (all ((<= (100 :: Natural.Natural)) . SearchBudget.mistakes . (`Opponent.budgetAt` MatchOptions.budget)) [minBound .. maxBound :: Difficulty.Difficulty]) "every rate a percent"

  Spec.describe s "weightsOf" $ do
    Spec.it s "weighs balanced as the neutral weights, every persona apart" $ do
      let personas = [minBound .. maxBound] :: [Persona.Persona]
      Spec.assertEq s (Opponent.weightsOf Persona.MkBalanced) Weights.neutral
      Spec.assertBool s (and [Opponent.weightsOf x /= Opponent.weightsOf y | x <- personas, y <- personas, x /= y]) "every persona weighed apart"

  Spec.describe s "named" $ do
    Spec.it s "reads each difficulty and persona back by its name, every name apart" $ do
      let levels = [minBound .. maxBound] :: [Difficulty.Difficulty]
          personas = [minBound .. maxBound] :: [Persona.Persona]
      Spec.assertEq s (traverse (Opponent.named Opponent.difficultyName . fst . Opponent.difficultyName) levels) (Just levels)
      Spec.assertEq s (traverse (Opponent.named Opponent.personaName . fst . Opponent.personaName) personas) (Just personas)
      Spec.assertEq s (Opponent.named Opponent.difficultyName "impossible") Nothing
      Spec.assertEq s (length (ListUtils.nubOrd (fmap (fst . Opponent.difficultyName) levels))) (length levels)
      Spec.assertEq s (length (ListUtils.nubOrd (fmap (fst . Opponent.personaName) personas))) (length personas)
