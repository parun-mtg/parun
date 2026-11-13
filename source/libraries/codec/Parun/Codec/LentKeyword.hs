{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LentKeyword where

import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.ActivationRestriction as ActivationRestriction
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LentKeyword as LentKeyword

-- | A bare object keyed by the record's field names; `instances` defaults to
-- one, `triggered` to false and `restrictions` to none.
codec :: Codec.Codec LentKeyword.LentKeyword
codec = Fields.object $ do
  keyword <- Fields.required "keyword" Keyword.codec LentKeyword.keyword
  instances <- Fields.defaulted "instances" 1 Common.natural LentKeyword.instances
  lender <- Fields.required "lender" AbilityName.codec LentKeyword.lender
  triggered <- Fields.defaulted "triggered" False Common.boolean LentKeyword.triggered
  restrictions <- Fields.defaulted "restrictions" [] (Common.list ActivationRestriction.codec) LentKeyword.restrictions
  pure LentKeyword.MkLentKeyword {LentKeyword.keyword = keyword, LentKeyword.instances = instances, LentKeyword.lender = lender, LentKeyword.triggered = triggered, LentKeyword.restrictions = restrictions}
