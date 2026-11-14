module Parun.Types.LifeGainR where

import qualified Parun.Types.ControllerRelation as ControllerRelation
import qualified Parun.Types.LifeGainCause as LifeGainCause
import qualified Parun.Types.LifeGainRewrite as LifeGainRewrite

-- | The payload of Parun.Types.ReplacementEffect's LifeGainR arm: whose life
-- gains are intercepted (CR 119.10 / 614.1a), what caused them, and what
-- happens instead.
data LifeGainR = MkLifeGainR
  { whose :: ControllerRelation.ControllerRelation,
    -- | CR 609.1: the cause the gain must have; AnyCause unless a clause
    -- narrows it (Rain of Gore).
    whichCause :: LifeGainCause.LifeGainCause,
    rewrite :: LifeGainRewrite.LifeGainRewrite
  }
  deriving (Eq, Ord, Show)
