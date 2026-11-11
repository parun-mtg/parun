{-# LANGUAGE GADTs #-}

module Parun.Types.StoredKind where

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

-- | Which of Parun.Types.StoredEffects' lists a row of type @r@ is from, one
-- constructor per field, so a function asked of every row cases on it
-- exhaustively.
data StoredKind r where
  -- | StoredEffects.continuousEffects (CR 611.2).
  Continuous :: StoredKind (ContinuousEffect.ContinuousEffect Card.Card)
  -- | StoredEffects.copyEffects (CR 613.1a).
  Copy :: StoredKind ActiveCopy.ActiveCopy
  -- | StoredEffects.replacements (CR 614.3).
  Replacement :: StoredKind ActiveReplacement.ActiveReplacement
  -- | StoredEffects.playerEffects (CR 613.11).
  PlayerEffect :: StoredKind ActivePlayerEffect.ActivePlayerEffect
  -- | StoredEffects.blockRequirements (CR 509.1c).
  BlockRequirement :: StoredKind ActiveBlockRequirement.ActiveBlockRequirement
  -- | StoredEffects.attackRequirements (CR 508.1d).
  AttackRequirement :: StoredKind ActiveAttackRequirement.ActiveAttackRequirement
  -- | StoredEffects.objectProhibitions (CR 611.2a).
  ObjectProhibition :: StoredKind ActiveObjectProhibition.ActiveObjectProhibition
  -- | StoredEffects.attackProhibitions (CR 508.1c).
  AttackProhibition :: StoredKind ActiveAttackProhibition.ActiveAttackProhibition
  -- | StoredEffects.evasions (CR 509.1b).
  Evasion :: StoredKind ActiveEvasion.ActiveEvasion
  -- | StoredEffects.ignoredAbilities (CR 116.2d).
  IgnoredAbility :: StoredKind IgnoredAbility.IgnoredAbility
  -- | StoredEffects.laterPayments (CR 116.2c).
  LaterPayment :: StoredKind ActiveLaterPayment.ActiveLaterPayment
  -- | StoredEffects.delayedTriggers (CR 603.7).
  DelayedTrigger :: StoredKind DelayedTrigger.DelayedTrigger
  -- | StoredEffects.pendingPreventionRiders (CR 615.5).
  PendingPreventionRider :: StoredKind Prevention.Prevention
  -- | StoredEffects.pendingReplacementEffects (CR 614.1a).
  PendingReplacementEffect :: StoredKind PendingReplacementEffect.PendingReplacementEffect
  -- | StoredEffects.pendingSkipEffects (CR 614.10b).
  PendingSkipEffect :: StoredKind PendingSkipEffect.PendingSkipEffect
  -- | StoredEffects.pendingEntryEffects (CR 614.1c).
  PendingEntryEffect :: StoredKind PendingEntryEffect.PendingEntryEffect
