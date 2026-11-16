module Parun.Types.Repeat where

import qualified Data.Sequence as Seq
import qualified Parun.Types.Again as Again
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.PlayerRef as PlayerRef

-- | A process run again after each run: CR 608.2d's "may repeat this process",
-- a player's choice -- Kindle the Carnage's "you", Trade Secrets' "that
-- opponent" -- or CR 608.2c's counted "repeat this process X more times",
-- Another Round's.
--
-- Parametric in the effect for Parun.Types.ForEach's reason.
data Repeat effect = MkRepeat
  { -- | Who is asked after each run, or how many more runs there are.
    again :: Again.Again,
    -- | The process, run in written order (CR 608.2c) before each ask.
    body :: Seq.Seq effect,
    -- | CR 706.3c: what must hold, after a run, for the chooser to be asked --
    -- Delina, Wild Mage's "15--20 | ... You may roll again". Absent asks every
    -- time. Read only beside Again.AskPlayer; Parun.EffectLintSpec rejects one
    -- beside a count.
    gate :: Maybe Condition.Condition,
    -- | Who is asked instead when the gate fails -- Game of Chaos's "that
    -- player decides". Absent ends the loop unasked. Read only beside a gate;
    -- Parun.EffectLintSpec rejects one without.
    otherChooser :: Maybe PlayerRef.PlayerRef,
    -- | What going again does before the next run, in written order -- Game of
    -- Chaos's "double the life stakes". A slot only this binds survives the
    -- per-run rescope.
    ifAgain :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
