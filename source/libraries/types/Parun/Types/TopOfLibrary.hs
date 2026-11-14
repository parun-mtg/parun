module Parun.Types.TopOfLibrary where

import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SideDeck as SideDeck

-- | The top cards of a library, named by whose library and how many. CR 401.2
-- makes a library an ordered pile and CR 121.1 names its head the top, which
-- together are what a POSITION in one means.
--
-- The depth is a Quantity rather than a Natural, so Commune with Lava's "exile
-- the top X cards of your library" is sayable beside Act on Impulse's literal
-- three. A computed depth is read when the effect executes (CR 608.2h), and
-- Parun.Engine.Resolve.Slots.objectRefObjects clamps an unevaluable or negative answer
-- to zero (CR 107.2, 107.1b).
--
-- A depth that a FILTER ends instead of a number is
-- Parun.Types.TopOfLibraryUntil.
data TopOfLibrary = MkTopOfLibrary
  { player :: PlayerRef.PlayerRef,
    count :: Quantity.Quantity,
    -- | Nothing for the library; a side deck's top otherwise (CR 901.3 / 311.7:
    -- Pools of Becoming's "the top three cards of your planar deck").
    deck :: Maybe SideDeck.SideDeck
  }
  deriving (Eq, Ord, Show)
