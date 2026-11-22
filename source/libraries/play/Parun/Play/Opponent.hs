-- | The search bot's settings named for a person choosing an opponent: a
-- difficulty's budget and a persona's weights (#9348). Parun.Play.OpponentSpec
-- proves their order; the rates each level wins at against the one below
-- are measured with `parun match --level`.
module Parun.Play.Opponent where

import qualified Data.List as List
import qualified Parun.Types.Difficulty as Difficulty
import qualified Parun.Types.Features as Features
import qualified Parun.Types.Persona as Persona
import qualified Parun.Types.SearchBudget as SearchBudget
import qualified Parun.Types.Weights as Weights

-- | `base` at a difficulty: the easiest playing each option out once, and
-- each easier level making more mistakes; how far a playout looks kept, so
-- a level is no slower wherever it is chosen than `base` is. The hardest is
-- `base` making none.
budgetAt :: Difficulty.Difficulty -> SearchBudget.SearchBudget -> SearchBudget.SearchBudget
budgetAt difficulty base = case difficulty of
  Difficulty.MkEasy -> base {SearchBudget.samples = 1, SearchBudget.mistakes = 40}
  Difficulty.MkMedium -> base {SearchBudget.mistakes = 15}
  Difficulty.MkHard -> base {SearchBudget.mistakes = 0}

-- | A persona's weights (Parun.Play.Search.evaluate).
weightsOf :: Persona.Persona -> Weights.Weights
weightsOf persona = case persona of
  Persona.MkBalanced -> Weights.neutral
  Persona.MkAggro -> Weights.MkWeights {Weights.ours = alike {Features.power = 3}, Weights.theirs = alike {Features.life = 3}}
  Persona.MkControl -> Weights.MkWeights {Weights.ours = alike {Features.life = 2, Features.hand = 3, Features.toughness = 2}, Weights.theirs = alike {Features.power = 3, Features.permanents = 2}}
  where
    alike = Weights.ours Weights.neutral

-- | A difficulty as a flag and a stored setting name it, and as the page
-- shows it.
difficultyName :: Difficulty.Difficulty -> (String, String)
difficultyName difficulty = case difficulty of
  Difficulty.MkEasy -> ("easy", "Easy")
  Difficulty.MkMedium -> ("medium", "Medium")
  Difficulty.MkHard -> ("hard", "Hard")

-- | A persona as a flag and a stored setting name it, and as the page shows
-- it.
personaName :: Persona.Persona -> (String, String)
personaName persona = case persona of
  Persona.MkBalanced -> ("balanced", "Balanced")
  Persona.MkAggro -> ("aggro", "Aggro")
  Persona.MkControl -> ("control", "Control")

-- | The value whose first name, by `nameOf`, is this.
named :: (Bounded a, Enum a) => (a -> (String, String)) -> String -> Maybe a
named nameOf name = List.find ((== name) . fst . nameOf) [minBound .. maxBound]
