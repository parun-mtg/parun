module Parun.Types.CausedBy where

import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.PlayerRelation as PlayerRelation

-- | CR 603.2 / 609.1: a trigger condition narrowed to the events a spell or
-- ability's effect caused, whose controller relates thus to CR 109.5's "you" --
-- Sacred Ground's "a spell or ability an opponent controls causes a land to be
-- put into your graveyard from the battlefield".
--
-- PARAMETRIC in the condition it narrows, Parun.Types.PlayerActed's posture, so
-- Parun.Types.TriggerCondition can hold one without a module cycle.
data CausedBy condition = MkCausedBy
  { controller :: PlayerRelation.PlayerRelation,
    -- | CR 702.72c: Just narrows further, to an ability of the BEARER that a
    -- keyword of this family represents -- Mistbind Clique's "championed with
    -- this creature". Nothing admits any spell or ability.
    sourceKeyword :: Maybe KeywordFamily.KeywordFamily,
    condition :: condition
  }
  deriving (Eq, Ord, Show)
