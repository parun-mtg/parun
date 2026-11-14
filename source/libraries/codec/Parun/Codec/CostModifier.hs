{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CostModifier where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.CostChange as CostChange
import qualified Parun.Codec.CostSubject as CostSubject
import qualified Parun.Codec.CountedAgainst as CountedAgainst
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PerTarget as PerTarget
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.Codec.TurnScope as TurnScope
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActivationCriteria as ActivationCriteria
import qualified Parun.Types.CostChange as CostChange.Type
import qualified Parun.Types.CostModifier as CostModifier
import qualified Parun.Types.CostSubject as CostSubject.Type
import qualified Parun.Types.CountedAgainst as CountedAgainst.Type

-- | A bare object keyed by the record's field names. The four Maybe criteria
-- are DEFAULTED to Nothing, a sentence that names no target, no "for each"
-- and no "first", so a card file writes only the ones it prints;
-- `countedAgainst` to the change's source, and refused beside no `perEach`.
--
-- Refused ('Fields.objectWith''s check): `onlyFirst` beside `whichLoyalty`, which no card
-- exercises -- Scryfall o:"first" o:"loyalty abilit" o:"cost", 2026-10-10,
-- finds no printing combining them; and a target criterion on an
-- addition to an activation, whose components Parun.Engine.Activate measures
-- before CR 601.2c announces the targets -- Scryfall o:"abilities"
-- o:"activate that target" o:"additional", 2026-10-10, no printing.
codec :: Codec.Codec CostModifier.CostModifier
codec = Fields.objectWith check $ do
  subject <- Fields.required "subject" CostSubject.codec CostModifier.subject
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) CostModifier.matching
  whichTargets <- Fields.defaulted "whichTargets" Nothing (Common.maybe (Filter.codec Keyword.codec)) CostModifier.whichTargets
  perTarget <- Fields.defaulted "perTarget" Nothing (Common.maybe PerTarget.codec) CostModifier.perTarget
  perEach <- Fields.defaulted "perEach" Nothing (Common.maybe Quantity.codec) CostModifier.perEach
  countedAgainst <- Fields.defaulted "countedAgainst" CountedAgainst.Type.ChangeSource CountedAgainst.codec CostModifier.countedAgainst
  onlyFirst <- Fields.defaulted "onlyFirst" Nothing (Common.maybe TurnScope.codec) CostModifier.onlyFirst
  change <- Fields.required "change" CostChange.codec CostModifier.change
  pure
    CostModifier.MkCostModifier
      { CostModifier.subject = subject,
        CostModifier.matching = matching,
        CostModifier.whichTargets = whichTargets,
        CostModifier.perTarget = perTarget,
        CostModifier.perEach = perEach,
        CostModifier.countedAgainst = countedAgainst,
        CostModifier.onlyFirst = onlyFirst,
        CostModifier.change = change
      }
  where
    check modifier = case (CostModifier.perEach modifier, CostModifier.countedAgainst modifier) of
      (Nothing, CountedAgainst.Type.PaidFor) -> Left (Text.pack "CostModifier: countedAgainst without perEach counts nothing")
      _ -> onlyFirstCheck modifier
    onlyFirstCheck modifier = case (CostModifier.subject modifier, CostModifier.onlyFirst modifier) of
      (_, Nothing) -> additionCheck modifier
      (CostSubject.Type.Spells, Just _) -> additionCheck modifier
      (CostSubject.Type.Activations criteria, Just _)
        | Just _ <- ActivationCriteria.whichLoyalty criteria -> Left (Text.pack "CostModifier: onlyFirst beside whichLoyalty has no printing")
        | otherwise -> additionCheck modifier
    additionCheck modifier = case (CostModifier.subject modifier, CostModifier.change modifier) of
      (CostSubject.Type.Activations _, CostChange.Type.Add _)
        | Maybe.isJust (CostModifier.whichTargets modifier) || Maybe.isJust (CostModifier.perTarget modifier) -> Left (Text.pack "CostModifier: an addition to an activation cannot read its targets")
      _ -> Right modifier
