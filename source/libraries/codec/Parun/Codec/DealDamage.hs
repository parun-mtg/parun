{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DealDamage where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.DamagePart as DamagePart
import qualified Parun.Codec.ExcessDestination as ExcessDestination
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DealDamage as DealDamage

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.Effect's DealDamage arm.
--
-- A sweep of @dealers@ binds each under @dealer@'s name, so it is refused
-- without one.
--
-- Not implemented: a sweep beside @excess@ or @excessTally@, which several
-- dealers would misread (#7272); it is refused.
codec :: Codec.Codec DealDamage.DealDamage
codec = Fields.objectWith sweepIsNamed $ do
  parts <- Fields.required "parts" (Common.seq DamagePart.codec) DealDamage.parts
  dealer <- Fields.defaulted "dealer" Nothing (Common.maybe SlotName.codec) DealDamage.dealer
  dealers <- Fields.defaulted "dealers" Nothing (Common.maybe ObjectRef.codec) DealDamage.dealers
  -- Defaulted to Nothing, so every card that deals damage without saying where
  -- the excess goes -- which is every one of them but Flame Spill -- keeps
  -- parsing unchanged.
  excess <- Fields.defaulted "excess" Nothing (Common.maybe ExcessDestination.codec) DealDamage.excess
  tally <- Fields.defaulted "tally" Nothing (Common.maybe SlotName.codec) DealDamage.tally
  damaged <- Fields.defaulted "damaged" Nothing (Common.maybe SlotName.codec) DealDamage.damaged
  excessTally <- Fields.defaulted "excessTally" Nothing (Common.maybe SlotName.codec) DealDamage.excessTally
  pure
    DealDamage.MkDealDamage
      { DealDamage.parts = parts,
        DealDamage.dealer = dealer,
        DealDamage.dealers = dealers,
        DealDamage.excess = excess,
        DealDamage.tally = tally,
        DealDamage.damaged = damaged,
        DealDamage.excessTally = excessTally
      }

sweepIsNamed :: DealDamage.DealDamage -> Either Text.Text DealDamage.DealDamage
sweepIsNamed damage
  | Maybe.isJust (DealDamage.dealers damage) && Maybe.isNothing (DealDamage.dealer damage) = Left (Text.pack "DealDamage: dealers needs a dealer to bind each one under")
  | Maybe.isJust (DealDamage.dealers damage) && (Maybe.isJust (DealDamage.excess damage) || Maybe.isJust (DealDamage.excessTally damage)) = Left (Text.pack "DealDamage: dealers beside excess or excessTally is not implemented (#7272)")
  | otherwise = Right damage
