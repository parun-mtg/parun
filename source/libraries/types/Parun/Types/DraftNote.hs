module Parun.Types.DraftNote where

import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

-- | CR 905.2b: one piece of information a player noted during the draft for a
-- card they drafted.
data DraftNote
  = -- | A number (Lurking Automaton, Cogwork Grinder). CR 905.2b.
    Number Natural.Natural
  | -- | A colour (Paliano, the High City; Regicide). CR 905.2b.
    Color Color.Color
  | -- | A card's name (Smuggler Captain, Noble Banneret). CR 905.2b.
    Name CardName.CardName
  | -- | One creature type (Paliano Vanguard), each of a card's noted alone. CR 905.2b.
    CreatureType Subtype.Subtype
  | -- | A player, by the seat they sit in this game (Cogwork Tracker). CR 905.2b.
    Player PlayerId.PlayerId
  deriving (Eq, Ord, Show)
