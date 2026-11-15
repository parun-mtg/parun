module Parun.Codec.CastingPermission where

import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.CostComponent as CostComponent
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.Codec.WithCounters as WithCounters
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CastingPermission as CastingPermission

codec :: Codec.Codec CastingPermission.CastingPermission
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "CastFromLibraryWhileSearching" CastingPermission.CastFromLibraryWhileSearching,
      Arm.payload "CastFromGraveyard" graveyard (\(f, c, w) -> CastingPermission.CastFromGraveyard f c w) (\x -> case x of CastingPermission.CastFromGraveyard f c w -> Just (f, c, w); _ -> Nothing),
      Arm.nullary "PlayFromGraveyard" CastingPermission.PlayFromGraveyard,
      Arm.payload "CastAsThoughItHadFlash" flash (\(c, m, a) -> CastingPermission.CastAsThoughItHadFlash c m a) (\x -> case x of CastingPermission.CastAsThoughItHadFlash c m a -> Just (c, m, a); _ -> Nothing)
    ]
  where
    graveyard =
      Fields.object $
        (,,)
          <$> Fields.defaulted "using" Nothing (Common.maybe KeywordFamily.codec) (\(f, _, _) -> f)
          <*> Fields.defaulted "additionalCosts" [] (Common.list (CostComponent.codec Keyword.codec)) (\(_, c, _) -> c)
          <*> Fields.defaulted "entersWith" Nothing (Common.maybe WithCounters.codec) (\(_, _, w) -> w)
    flash =
      Fields.object $
        (,,)
          <$> Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) (\(c, _, _) -> c)
          <*> Fields.defaulted "additionalCost" Nothing (Common.maybe ManaCost.codec) (\(_, m, _) -> m)
          <*> Fields.defaulted "outsideSorceryTiming" Nothing (Common.maybe AbilityName.codec) (\(_, _, a) -> a)

tagOf :: CastingPermission.CastingPermission -> String
tagOf x = case x of
  CastingPermission.CastFromLibraryWhileSearching {} -> "CastFromLibraryWhileSearching"
  CastingPermission.CastFromGraveyard {} -> "CastFromGraveyard"
  CastingPermission.PlayFromGraveyard {} -> "PlayFromGraveyard"
  CastingPermission.CastAsThoughItHadFlash {} -> "CastAsThoughItHadFlash"
