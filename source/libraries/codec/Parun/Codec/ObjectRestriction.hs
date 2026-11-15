{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ObjectRestriction where

import qualified Parun.Codec.Affected as Affected
import qualified Parun.Codec.RestrictedAction as RestrictedAction
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ObjectRestriction as ObjectRestriction

-- | An object with two named keys, the shape Parun.Codec.AttackRequirement has and
-- for its reason: Parun.Types.ObjectRestriction is a record, so there is no sum
-- for a tag to discriminate. Parun.Codec.CombatRestriction is tagged because
-- that type has six arms.
--
-- The key is "affected" and not "subject": CR 101.2's prohibition names the
-- permanents it restricts, which is the word CombatRestriction's payload spells,
-- where a requirement's "subject" is opposed to an object it acts on.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec ObjectRestriction.ObjectRestriction
codec = Fields.object $ do
  action <- Fields.required "action" RestrictedAction.codec ObjectRestriction.action
  affected <- Fields.required "affected" Affected.codec ObjectRestriction.affected
  pure ObjectRestriction.MkObjectRestriction {ObjectRestriction.action = action, ObjectRestriction.affected = affected}
