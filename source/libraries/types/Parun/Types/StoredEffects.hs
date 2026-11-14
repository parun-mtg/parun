module Parun.Types.StoredEffects where

import qualified Data.Sequence as Seq
import qualified Parun.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Parun.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Parun.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Parun.Types.ActiveCopy as ActiveCopy
import qualified Parun.Types.ActiveEvasion as ActiveEvasion
import qualified Parun.Types.ActiveLaterPayment as ActiveLaterPayment
import qualified Parun.Types.ActiveObjectProhibition as ActiveObjectProhibition
import qualified Parun.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Parun.Types.ActiveReplacement as ActiveReplacement
import qualified Parun.Types.Card as Card
import qualified Parun.Types.ContinuousEffect as ContinuousEffect
import qualified Parun.Types.DelayedTrigger as DelayedTrigger
import qualified Parun.Types.IgnoredAbility as IgnoredAbility
import qualified Parun.Types.PendingEntryEffect as PendingEntryEffect
import qualified Parun.Types.PendingReplacementEffect as PendingReplacementEffect
import qualified Parun.Types.PendingSkipEffect as PendingSkipEffect
import qualified Parun.Types.Prevention as Prevention

-- | CR 611.2: the rows resolutions stored, held whole as GameState.stored. Each
-- kind keeps its own list, in its own order, so a reader of one kind never
-- scans another. Parun.Engine.Stored.traverseRows matches it positionally, so
-- a new kind is a build error there, and through Parun.Types.StoredKind in
-- every sweep and scan that walks every row.
data StoredEffects = MkStoredEffects
  { -- | CR 611.2: stored continuous effects from resolutions, each with an
    -- expiry; static-ability effects are re-derived live instead.
    continuousEffects :: [ContinuousEffect.ContinuousEffect Card.Card],
    -- | CR 613.1a / 611.2: stored copy effects from resolutions that stated a
    -- duration, each with an expiry. A copy effect stating none is stamped onto
    -- the object instead (Parun.Engine.Binding.setCopy), which is what has
    -- nowhere to record an ending.
    copyEffects :: [ActiveCopy.ActiveCopy],
    -- | CR 614.3 / 615.3: floating replacement effects from resolutions, each
    -- with an expiry and a use count; static replacement abilities are
    -- re-derived live instead.
    replacements :: [ActiveReplacement.ActiveReplacement],
    -- | CR 611.1 / 613.11: stored player- and rules-modifying continuous effects
    -- from resolutions, each with an expiry; printed ones are re-derived live.
    playerEffects :: [ActivePlayerEffect.ActivePlayerEffect],
    -- | CR 509.1c / 613.11: stored blocking requirements from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    blockRequirements :: [ActiveBlockRequirement.ActiveBlockRequirement],
    -- | CR 508.1d / 613.11: stored attacking requirements from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    attackRequirements :: [ActiveAttackRequirement.ActiveAttackRequirement],
    -- | CR 611.2a / 613.11: stored per-object prohibitions from resolutions,
    -- each with an expiry; printed ones are re-derived live.
    objectProhibitions :: [ActiveObjectProhibition.ActiveObjectProhibition],
    -- | CR 508.1c / 611.1: stored attacking restrictions from resolutions, each
    -- with an expiry; printed ones are re-derived live.
    attackProhibitions :: [ActiveAttackProhibition.ActiveAttackProhibition],
    -- | CR 509.1b / 611.2c: stored classes of attackers that can't be
    -- blocked, from resolutions, each with an expiry.
    evasions :: [ActiveEvasion.ActiveEvasion],
    -- | CR 116.2d: the ignores players have paid for, each with an expiry, read
    -- by Parun.Engine.PlayerEffect.applying.
    ignoredAbilities :: [IgnoredAbility.IgnoredAbility],
    -- | CR 116.2c: what a resolution left a player to pay for later, each row
    -- offered until its expiry.
    laterPayments :: [ActiveLaterPayment.ActiveLaterPayment],
    -- | CR 603.7: delayed triggered abilities awaiting their event, in creation
    -- order; not cleared at turn handoff.
    delayedTriggers :: Seq.Seq DelayedTrigger.DelayedTrigger,
    -- | CR 615.5: prevention applications whose additional effect has not run
    -- yet, drained by Resolve.runPreventionRiders before the next state-based
    -- action check (CR 704.3). Empty at every priority window.
    pendingPreventionRiders :: Seq.Seq Prevention.Prevention,
    -- | CR 614.1a: the effects a replacement applied in an event's place and
    -- that have not run yet -- a damage event's (DamageRewrite.RunEffects), a
    -- zone change's (ZoneChangeR.riders), a loss of the game's (GameLossR) or a
    -- draw's (DrawRewrite.FromGraveyardOrLose, DrawRewrite.RunEffects)
    -- -- drained by Resolve.runReplacementEffects.
    --
    -- Empty at every priority window because every road that can fill it drains
    -- it: for damage, each road into Parun.Engine.Damage.applyDamage (Resolve's
    -- Effect.DealDamage and Effect.Fight arms, and Parun.Engine.Engine's combat
    -- damage step); for a loss, a draw or a zone change, the state-based
    -- action pass that met it (Sba.checkOnce, Engine.settlePasses) and each
    -- resolved instruction
    -- (Resolve.applyEffectWith), and each draw of a sequence, an instruction's
    -- or a replacement's (Event.drawSeries).
    pendingReplacementEffects :: Seq.Seq PendingReplacementEffect.PendingReplacementEffect,
    -- | CR 614.10b: the actions a skip put first in the next step, phase or turn
    -- to actually occur (PhaseRewrite.MaySkipFor), drained by
    -- Parun.Engine.Engine.startStep as that step begins.
    pendingSkipEffects :: Seq.Seq PendingSkipEffect.PendingSkipEffect,
    -- | CR 614.1c: as-enters rewrites whose effects have not run yet. Drained as
    -- the instruction that entered the permanent finishes
    -- (Parun.Engine.Resolve.Effect.applyEffectWith), and first thing in
    -- Parun.Engine.Engine.performSettle for every other road; either way the entry's
    -- CR 603.10 sample is retaken after them. Empty at every priority window.
    pendingEntryEffects :: Seq.Seq PendingEntryEffect.PendingEntryEffect
  }
  deriving (Eq, Ord, Show)
