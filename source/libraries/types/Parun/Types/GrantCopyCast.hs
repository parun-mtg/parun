module Parun.Types.GrantCopyCast where

import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.Duration as Duration
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.PlayerRef as PlayerRef

-- | The payload of Parun.Types.Effect's GrantCopyCast arm: a standing CR 601.3
-- permission to cast a copy (CR 707.12) of the card whose name the resolution
-- chose (CR 201.4), created outside the game as CR 707.13's is -- Discord, Lord
-- of Disharmony's "until your next end step, you may cast a copy of a spell
-- with that name".
data GrantCopyCast = MkGrantCopyCast
  { duration :: Duration.Duration,
    -- | WHO may cast it, Parun.Types.GrantPlayFromExile's `player`.
    player :: PlayerRef.PlayerRef,
    -- | CR 118.14's "and mana of any type can be spent to cast it".
    spending :: ManaSpending.ManaSpending,
    -- | CR 707.10: "if you cast a spell this way, copy this ability if" the
    -- condition holds, read off the ability's source as the spell becomes cast
    -- (CR 601.2i). Nothing copies nothing.
    recopy :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
