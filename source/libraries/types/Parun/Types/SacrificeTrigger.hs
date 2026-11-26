module Parun.Types.SacrificeTrigger where

import qualified Parun.Types.Batching as Batching
import qualified Parun.Types.PlayerActsOn as PlayerActsOn
import qualified Parun.Types.SacrificeCause as SacrificeCause

-- | CR 603.2 over CR 701.21a's sacrifice: whose, of what, and per permanent or
-- per sacrifice event (CR 603.2c), and which sacrifices count.
data SacrificeTrigger = MkSacrificeTrigger
  { acts :: PlayerActsOn.PlayerActsOn,
    batching :: Batching.Batching,
    -- | Nothing for every sacrifice; Just Devour for CR 702.82b's "is devoured"
    -- (Kresh the Bloodbraided Avatar).
    --
    -- Unprinted: Just Ordinary, a trigger on every sacrifice but a devoured
    -- one. Parun.EffectLintSpec's "no card narrows a sacrifice trigger to
    -- ordinary sacrifices" keeps it out of card data.
    cause :: Maybe SacrificeCause.SacrificeCause
  }
  deriving (Eq, Ord, Show)
