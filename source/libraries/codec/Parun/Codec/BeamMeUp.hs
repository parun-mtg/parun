{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.BeamMeUp where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Cost as Cost
import qualified Parun.Codec.Filter as Filter
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.BeamMeUp as BeamMeUp

-- | The Cost's own keys with @unreturnable@ beside them, omitted when Nothing,
-- so a printed beam me up is spelled as the bare Cost it states.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (BeamMeUp.BeamMeUp keyword)
codec keywordCodec = Fields.object $ do
  cost <- Fields.contramap BeamMeUp.cost (Cost.fields keywordCodec)
  unreturnable <- Fields.defaulted "unreturnable" Nothing (Common.maybe (Filter.codec keywordCodec)) BeamMeUp.unreturnable
  pure BeamMeUp.MkBeamMeUp {BeamMeUp.cost = cost, BeamMeUp.unreturnable = unreturnable}
