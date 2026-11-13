module Parun.Types.ForbiddenAct where

import qualified Numeric.Natural as Natural

-- | What Parun.Types.PlayerEffect's Cant arm says a player can't do (CR 101.2):
-- the restrictions that name an act and nothing else, each asked of the player
-- ahead of the act by Parun.Engine.PlayerEffect.forbids.
data ForbiddenAct
  = -- | CR 119.7 / Giant Cindermaw, Platinum Emperion.
    --
    -- A STATIC restriction rather than a replacement, which is why
    -- Parun.Engine.Event.resolveLifeGain consults it AHEAD of the proposal;
    -- Parun.PlayerEffectSpec's GiantCindermaw group proves it.
    GainLife
  | -- | CR 119.8 / Platinum Emperion.
    --
    -- Damage is still DEALT to such a player -- CR 120.3a's RESULT is what does
    -- not happen -- and CR 119.8's last sentence makes a pay-life cost
    -- unpayable. Parun.PlayerEffectSpec's PlatinumEmperion and GreedUnderEmperion
    -- groups prove the two.
    LoseLife
  | -- | CR 121.2b / 121.3 / Spirit of the Labyrinth: drawing more than this many
    -- cards each turn; 0 is Mornsong Aria's "can't draw cards".
    --
    -- data\/scenarios\/cant-draw proves the draw step's draw, an effect's, a
    -- replaced one, an offered one and the limit's.
    DrawMoreThan Natural.Natural
  | -- | CR 726.1 / 101.2 / Jared Carthalion, True Heir.
    BecomeMonarch
  deriving (Eq, Ord, Show)
