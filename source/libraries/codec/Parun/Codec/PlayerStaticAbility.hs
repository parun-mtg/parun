{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PlayerStaticAbility where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.PlayerEffect as PlayerEffect
import qualified Parun.Codec.PlayerScope as PlayerScope
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Parun.Types.Zone as Zone.Type

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- The "as long as" gate (CR 611.3a) is OPTIONAL, and absent means
-- unconditional -- Parun.Codec.StaticAbility's posture for the same field on
-- the object-facing carrier, so a card that states no clause writes no key. CR
-- 116.2d's name is optional for the same reason read the other way: only a face
-- whose own text refers to the ability writes one. So is @functionsFrom@,
-- empty for an ability stating no zone.
--
-- Unprinted: a @functionsFrom@ naming a zone other than Battlefield or
-- Command. It is refused, and Parun.Codec.PlayerStaticAbilitySpec's "rejects
-- a zone" proves the refusal.
codec :: Codec.Codec PlayerStaticAbility.PlayerStaticAbility
codec = Fields.objectWith zonesArePrinted $ do
  scope <- Fields.required "scope" PlayerScope.codec PlayerStaticAbility.scope
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) PlayerStaticAbility.condition
  functionsFrom <- Fields.defaulted "functionsFrom" Set.empty (Common.set Zone.codec) PlayerStaticAbility.functionsFrom
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) PlayerStaticAbility.name
  effect <- Fields.required "effect" PlayerEffect.codec PlayerStaticAbility.effect
  pure
    PlayerStaticAbility.MkPlayerStaticAbility
      { PlayerStaticAbility.scope = scope,
        PlayerStaticAbility.condition = condition,
        PlayerStaticAbility.functionsFrom = functionsFrom,
        PlayerStaticAbility.name = name,
        PlayerStaticAbility.effect = effect
      }

zonesArePrinted :: PlayerStaticAbility.PlayerStaticAbility -> Either Text.Text PlayerStaticAbility.PlayerStaticAbility
zonesArePrinted ability
  | all (`elem` [Zone.Type.Battlefield, Zone.Type.Command]) (PlayerStaticAbility.functionsFrom ability) = Right ability
  | otherwise = Left (Text.pack "PlayerStaticAbility: a functionsFrom zone other than Battlefield or Command is unprinted and not supported")
