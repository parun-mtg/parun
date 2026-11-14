module Parun.Types.ArrangeInLibrary where

import qualified Parun.Types.LibraryPosition as LibraryPosition
import qualified Parun.Types.ObjectRef as ObjectRef
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Effect's ArrangeInLibrary arm: WHICH cards go
-- back in an order, and WHO states it. The chooser is the player the
-- instruction addresses (CR 608.2c), not the library's owner: Cruel Fate's
-- caster orders the opponent's cards (Portent's ruling), and Tahngarth's
-- Glare's "that player ... puts them back in any order" names the opponent.
-- Parun.Types.ChosenCardFromAmong's chooser is the same PlayerRef.
data ArrangeInLibrary = MkArrangeInLibrary
  { ref :: ObjectRef.ObjectRef,
    chooser :: PlayerRef.PlayerRef,
    -- | Nothing leaves the cards at the positions they occupy (Ponder); an end
    -- moves them there, in the chooser's order -- Pools of Becoming's "put the
    -- revealed cards on the bottom of your planar deck in any order".
    end :: Maybe LibraryPosition.LibraryPosition
  }
  deriving (Eq, Ord, Show)
