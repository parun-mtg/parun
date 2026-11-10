module Parun.Types.PerformerR where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.InstructionRewrite as InstructionRewrite
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.PlayerInstruction as PlayerInstruction

-- | The payload of Parun.Types.ReplacementEffect's PerformerR arm: which
-- instruction a permanent carries out for its player is intercepted, the Filter
-- judging that permanent, and what happens instead (CR 614.1a).
--
-- InstructionR's shape with the permanent in place of the player: Steamflogger
-- Boss's "a Rigger you control would assemble" and Leader, Super-Genius's "a
-- creature you control would connive" name the performer, not a seat.
data PerformerR = MkPerformerR
  { instruction :: PlayerInstruction.PlayerInstruction,
    performer :: Filter.Filter Keyword.Keyword,
    rewrite :: InstructionRewrite.InstructionRewrite
  }
  deriving (Eq, Ord, Show)
