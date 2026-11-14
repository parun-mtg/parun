module Parun.Types.FromAnywhere where

import qualified Parun.Types.Reach as Reach
import qualified Parun.Types.SlotName as SlotName

-- | CR 400.7j / 731.3c: the objects a slot holds, followed to where they
-- moved -- Heart-Shaped Herb's "that card", Timmerian Fiends' "Timmerian
-- Fiends from anywhere".
--
-- The fields are named rather than positional, the shape every other
-- Parun.Types.ObjectRef payload record takes.
data FromAnywhere = MkFromAnywhere
  { slot :: SlotName.SlotName,
    -- | CR 400.7j: only an incarnation this other slot also holds -- Bronze
    -- Tablet's "this card", the member of its one exile (CR 608.2f) that was
    -- the Tablet, and nothing where the Tablet was not exiled.
    among :: Maybe SlotName.SlotName,
    -- | CR 400.7j: into a public zone only, or anywhere where the card prints
    -- "from anywhere".
    reach :: Reach.Reach
  }
  deriving (Eq, Ord, Show)
