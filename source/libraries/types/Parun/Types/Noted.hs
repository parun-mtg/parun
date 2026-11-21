module Parun.Types.Noted where

import qualified Data.Sequence as Seq
import qualified Parun.Types.ManaType as ManaType

-- | CR 607.2e: the information an ability last noted for an object, one
-- Parun.Types.NoteKind's worth.
data Noted
  = -- | NoteKind.ManaSpent's run of mana (Ice Cauldron).
    NotedMana (Seq.Seq ManaType.ManaType)
  | -- | NoteKind.LifeTotal's life total (Sigarda's Splendor).
    NotedLifeTotal Integer
  deriving (Eq, Ord, Show)
