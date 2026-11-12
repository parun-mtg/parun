{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CantBeBlockedBy where

import qualified Parun.Codec.AbilityName as AbilityName
import qualified Parun.Codec.Affected as Affected
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.PairwiseFrame as PairwiseFrame
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Parun.Types.PairwiseFrame as PairwiseFrame.Type

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name.
--
-- "blockers" and not a second "affected": the two creature-naming keys are not
-- interchangeable, and naming them is what stops a card file barring the
-- attackers from themselves.
--
-- "frame" defaults to Attacker, the reading of every printing but Champion of
-- Lambholt's. "name" defaults to Nothing, Parun.Codec.AffectedUnless's reason.
codec :: Codec.Codec CantBeBlockedBy.CantBeBlockedBy
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CantBeBlockedBy.affected
  blockers <- Fields.required "blockers" (Filter.codec Keyword.codec) CantBeBlockedBy.blockers
  frame <- Fields.defaulted "frame" PairwiseFrame.Type.Attacker PairwiseFrame.codec CantBeBlockedBy.frame
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) CantBeBlockedBy.unless
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) CantBeBlockedBy.name
  pure
    CantBeBlockedBy.MkCantBeBlockedBy
      { CantBeBlockedBy.affected = affected,
        CantBeBlockedBy.blockers = blockers,
        CantBeBlockedBy.frame = frame,
        CantBeBlockedBy.unless = unless,
        CantBeBlockedBy.name = name
      }
