{-# LANGUAGE RankNTypes #-}

-- CR 611.2: every row a resolution stored, walked once. The ONE place that
-- names Parun.Types.StoredEffects' fields: Parun.Engine.Expiry's sweeps and
-- Parun.Engine.Interchangeable's scan go through it, and each answers every
-- Parun.Types.StoredKind in one exhaustive case, so a new kind of row is a
-- build error in both until each says what the row means to it.
module Parun.Engine.Stored where

import qualified Data.Foldable as Foldable
import qualified Data.Functor.Const as Const
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Parun.Types.StoredEffects (StoredEffects)
import qualified Parun.Types.StoredEffects as StoredEffects
import Parun.Types.StoredKind (StoredKind)
import qualified Parun.Types.StoredKind as StoredKind

-- | Every row, kind by kind, each kind's survivors kept in its own order:
-- `edit` answers the row kept in its place, or Nothing to drop it. The
-- effects run in field order, which is the order Parun.Engine.Expiry's
-- CR 116.2c offers are listed in.
--
-- Matched POSITIONALLY, so a field added to StoredEffects is an arity error
-- here.
traverseRows :: (Applicative f) => (forall r. StoredKind r -> r -> f (Maybe r)) -> StoredEffects -> f StoredEffects
traverseRows edit stored = case stored of
  StoredEffects.MkStoredEffects continuous copy replacement player block attack objectProhibition attackProhibition evasion ignored later delayed prevention replacing skip entry ->
    StoredEffects.MkStoredEffects
      <$> list (edit StoredKind.Continuous) continuous
      <*> list (edit StoredKind.Copy) copy
      <*> list (edit StoredKind.Replacement) replacement
      <*> list (edit StoredKind.PlayerEffect) player
      <*> list (edit StoredKind.BlockRequirement) block
      <*> list (edit StoredKind.AttackRequirement) attack
      <*> list (edit StoredKind.ObjectProhibition) objectProhibition
      <*> list (edit StoredKind.AttackProhibition) attackProhibition
      <*> list (edit StoredKind.Evasion) evasion
      <*> list (edit StoredKind.IgnoredAbility) ignored
      <*> list (edit StoredKind.LaterPayment) later
      <*> queue (edit StoredKind.DelayedTrigger) delayed
      <*> queue (edit StoredKind.PendingPreventionRider) prevention
      <*> queue (edit StoredKind.PendingReplacementEffect) replacing
      <*> queue (edit StoredKind.PendingSkipEffect) skip
      <*> queue (edit StoredKind.PendingEntryEffect) entry

-- | traverseRows read rather than rebuilt: every row's answer, in that order.
foldRows :: (Monoid m) => (forall r. StoredKind r -> r -> m) -> StoredEffects -> m
foldRows answer = Const.getConst . traverseRows (\kind -> Const.Const . answer kind)

-- One kind's list under traverseRows' edit, survivors in order.
list :: (Applicative f) => (r -> f (Maybe r)) -> [r] -> f [r]
list edit = fmap Maybe.catMaybes . traverse edit

-- One kind's queue under traverseRows' edit, survivors in order.
queue :: (Applicative f) => (r -> f (Maybe r)) -> Seq.Seq r -> f (Seq.Seq r)
queue edit = fmap (Seq.fromList . Maybe.catMaybes) . traverse edit . Foldable.toList
