module Parun.Codec.CastingPermission where

import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.CostComponent as CostComponent
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.KeywordFamily as KeywordFamily
import qualified Parun.Codec.ManaCost as ManaCost
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
      Arm.payload "CastFromGraveyard" graveyard (uncurry CastingPermission.CastFromGraveyard) (\x -> case x of CastingPermission.CastFromGraveyard f c -> Just (f, c); _ -> Nothing),
      Arm.nullary "PlayFromGraveyard" CastingPermission.PlayFromGraveyard,
      Arm.payload "CastAsThoughItHadFlash" flash (\(c, m, k, a) -> CastingPermission.CastAsThoughItHadFlash c m k a) (\x -> case x of CastingPermission.CastAsThoughItHadFlash c m k a -> Just (c, m, k, a); _ -> Nothing)
    ]
  where
    graveyard =
      Fields.object $
        (,)
          <$> Fields.defaulted "using" Nothing (Common.maybe KeywordFamily.codec) fst
          <*> Fields.defaulted "additionalCosts" [] (Common.list (CostComponent.codec Keyword.codec)) snd
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
  CastingPermission.CastFromGraveyard {} -> "CastFromGraveyard"
  CastingPermission.PlayFromGraveyard {} -> "PlayFromGraveyard"
  CastingPermission.CastAsThoughItHadFlash {} -> "CastAsThoughItHadFlash"
