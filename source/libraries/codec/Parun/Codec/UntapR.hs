{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.UntapR where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.Phase as Phase
import qualified Parun.Codec.UntapRewrite as UntapRewrite
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.UntapR as UntapR

-- | The effect codec is a PARAMETER, Parun.Codec.DamageR's reason.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (UntapR.UntapR effect)
codec effectCodec = Fields.object $ do
  matching <- Fields.defaulted "matching" Nothing (Common.maybe (Filter.codec Keyword.codec)) UntapR.matching
  during <- Fields.defaulted "during" Nothing (Common.maybe Phase.codec) UntapR.during
  rewrite <- Fields.required "rewrite" (UntapRewrite.codec effectCodec) UntapR.rewrite
  pure
    UntapR.MkUntapR
      { UntapR.matching = matching,
        UntapR.during = during,
        UntapR.rewrite = rewrite
      }
