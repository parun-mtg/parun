{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.KeywordCostRestriction where

import qualified Parun.Codec.Affected as Affected
import qualified Parun.Codec.KeywordCostPart as KeywordCostPart
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.KeywordCostRestriction as KeywordCostRestriction

-- | An object with named keys: Parun.Types.KeywordCostRestriction is a record
-- with no sum for a tag to discriminate.
codec :: Codec.Codec KeywordCostRestriction.KeywordCostRestriction
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec KeywordCostRestriction.affected
  part <- Fields.required "part" KeywordCostPart.codec KeywordCostRestriction.part
  pure KeywordCostRestriction.MkKeywordCostRestriction {KeywordCostRestriction.part = part, KeywordCostRestriction.affected = affected}
