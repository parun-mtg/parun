module Parun.Types.SeatView where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Numeric.Natural as Natural
import qualified Parun.Types.DraftNote as DraftNote
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Mana as Mana
import qualified Parun.Types.ObjectView as ObjectView
import qualified Parun.Types.PlayerCounterKind as PlayerCounterKind
import qualified Parun.Types.PlayerId as PlayerId

-- | CR 400.2: one player's zones and totals as one seat sees them, a hidden
-- zone by its size and only the members that seat may see.
data SeatView = MkSeatView
  { player :: PlayerId.PlayerId,
    life :: Integer,
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural.Natural,
    handSize :: Natural.Natural,
    hand :: [ObjectView.ObjectView],
    librarySize :: Natural.Natural,
    library :: [ObjectView.ObjectView],
    graveyard :: [ObjectView.ObjectView],
    exile :: [ObjectView.ObjectView],
    manaPool :: Mana.Mana,
    -- | CR 905.2b: Player.draftNotes, which every seat may look at.
    draftNotes :: Map.Map CardName.CardName (Seq.Seq DraftNote.DraftNote)
  }
  deriving (Eq, Show)
