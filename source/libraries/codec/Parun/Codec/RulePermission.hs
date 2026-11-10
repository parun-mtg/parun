{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.RulePermission where

import qualified Parun.Codec.Affected as Affected
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.RuleWaiver as RuleWaiver
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.RulePermission as RulePermission
import qualified Parun.Types.RuleWaiver as RuleWaiver.Type

-- | An object with named keys, `while` omitted when Nothing, `waives` when
-- Defender.
codec :: Codec.Codec RulePermission.RulePermission
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec RulePermission.affected
  while <- Fields.defaulted "while" Nothing (Common.maybe Condition.codec) RulePermission.while
  waives <- Fields.defaulted "waives" RuleWaiver.Type.Defender RuleWaiver.codec RulePermission.waives
  pure RulePermission.MkRulePermission {RulePermission.affected = affected, RulePermission.while = while, RulePermission.waives = waives}
