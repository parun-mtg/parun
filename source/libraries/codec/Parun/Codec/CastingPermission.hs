module Parun.Codec.CastingPermission where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.CostComponent as CostComponent
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.Codec.WithCounters as WithCounters
import qualified Parun.Codec.Zone as Zone
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CastingPermission as CastingPermission
import qualified Parun.Types.Zone as Zone.Type

codec :: Codec.Codec CastingPermission.CastingPermission
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "CastFromLibraryWhileSearching" CastingPermission.CastFromLibraryWhileSearching,
      Arm.payload "CastFrom" castFrom (\(z, k, f, c, w) -> CastingPermission.CastFrom z k f c w) (\x -> case x of CastingPermission.CastFrom z k f c w -> Just (z, k, f, c, w); _ -> Nothing),
      Arm.nullary "PlayFromGraveyard" CastingPermission.PlayFromGraveyard,
      Arm.payload "CastAsThoughItHadFlash" flash (\(c, m, k, a) -> CastingPermission.CastAsThoughItHadFlash c m k a) (\x -> case x of CastingPermission.CastAsThoughItHadFlash c m k a -> Just (c, m, k, a); _ -> Nothing)
    ]
  where
    castFrom =
      Fields.objectWith printedZone $
        (,,,,)
          <$> Fields.required "from" Zone.codec (\(z, _, _, _, _) -> z)
          <*> Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) (\(_, k, _, _, _) -> k)
          <*> Fields.defaulted "using" Nothing (Common.maybe KeywordFamily.codec) (\(_, _, f, _, _) -> f)
          <*> Fields.defaulted "additionalCosts" [] (Common.list (CostComponent.codec Keyword.codec)) (\(_, _, _, c, _) -> c)
          <*> Fields.defaulted "entersWith" Nothing (Common.maybe WithCounters.codec) (\(_, _, _, _, w) -> w)
    flash =
      Fields.object $
        (,,,)
          <$> Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) (\(c, _, _, _) -> c)
          <*> Fields.defaulted "additionalMana" Nothing (Common.maybe ManaCost.codec) (\(_, m, _, _) -> m)
          <*> Fields.defaulted "additionalCosts" [] (Common.list (CostComponent.codec Keyword.codec)) (\(_, _, k, _) -> k)
          <*> Fields.defaulted "outsideSorceryTiming" Nothing (Common.maybe AbilityName.codec) (\(_, _, _, a) -> a)

tagOf :: CastingPermission.CastingPermission -> String
tagOf x = case x of
  CastingPermission.CastFromLibraryWhileSearching {} -> "CastFromLibraryWhileSearching"
  CastingPermission.CastFrom {} -> "CastFrom"
  CastingPermission.PlayFromGraveyard {} -> "PlayFromGraveyard"
  CastingPermission.CastAsThoughItHadFlash {} -> "CastAsThoughItHadFlash"

-- Unprinted: a CastFrom naming a zone other than a graveyard or exile, or exile
-- beside a condition, a family, a cost or counters, none of which the cast
-- gate or pricing reads outside a graveyard. Parun.Codec.CastingPermissionSpec's
-- "rejects" cases prove the refusal.
printedZone ::
  (Zone.Type.Zone, Maybe a, Maybe b, [c], Maybe d) ->
  Either Text.Text (Zone.Type.Zone, Maybe a, Maybe b, [c], Maybe d)
printedZone permission@(zone, condition, family, costs, entersWith) = case zone of
  Zone.Type.Graveyard -> Right permission
  Zone.Type.Exile
    | Maybe.isNothing condition && Maybe.isNothing family && null costs && Maybe.isNothing entersWith -> Right permission
  _ -> Left (Text.pack "CastFrom: a zone other than a graveyard or exile, or exile beside a condition, family, cost or counters, is unprinted and not supported")
