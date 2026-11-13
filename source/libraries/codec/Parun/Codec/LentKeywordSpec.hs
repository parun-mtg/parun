module Parun.Codec.LentKeywordSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.LentKeyword as LentKeyword
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AbilityName as AbilityName
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.LentKeyword as LentKeyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.LentKeyword" $ do
  Spec.it s "with every field" $
    Common.assertCodec
      s
      LentKeyword.codec
      LentKeyword.MkLentKeyword {LentKeyword.keyword = Keyword.Flanking, LentKeyword.instances = 2, LentKeyword.lender = AbilityName.MkAbilityName (Text.pack "acquiredFrom7"), LentKeyword.triggered = True, LentKeyword.restrictions = [ActivationRestriction.TimesEachTurn 1]}
      " {\"keyword\":{\"type\":\"Flanking\"},\"instances\":2,\"lender\":\"acquiredFrom7\",\"triggered\":true,\"restrictions\":[{\"type\":\"TimesEachTurn\",\"value\":1}]} "
  Spec.it s "defaults to one activated-only instance" $
    Common.assertCodec
      s
      LentKeyword.codec
      LentKeyword.MkLentKeyword {LentKeyword.keyword = Keyword.Flanking, LentKeyword.instances = 1, LentKeyword.lender = AbilityName.MkAbilityName (Text.pack "acquiredFrom7"), LentKeyword.triggered = False, LentKeyword.restrictions = []}
      " {\"keyword\":{\"type\":\"Flanking\"},\"lender\":\"acquiredFrom7\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LentKeyword.codec
