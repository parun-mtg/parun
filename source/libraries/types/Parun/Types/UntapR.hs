module Parun.Types.UntapR where

import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.UntapRewrite as UntapRewrite

-- | The payload of Parun.Types.ReplacementEffect's UntapR arm: when a permanent's
-- untap is intercepted, and what happens instead. Parametric in the EFFECT for
-- Parun.Types.UntapRewrite's RunEffects.
data UntapR effect = MkUntapR
  { -- | CR 614.1a: the printed subject (Edge of Malacol's "a creature you
    -- control"). Nothing is the row's own source, DestructionR's posture.
    matching :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 502.3: "during its controller's untap step" (Bewitching Leechcraft),
    -- as the step the game is in, its controller's own. Nothing is every untap
    -- (CR 122.1d).
    during :: Maybe Phase.Phase,
    rewrite :: UntapRewrite.UntapRewrite effect
  }
  deriving (Eq, Ord, Show)
