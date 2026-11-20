{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DestructionR where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.DestructionRewrite as DestructionRewrite
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DestructionR as DestructionR

-- | The effect codec is a PARAMETER, Parun.Codec.DamageR's reason.
codec ::
  (Typeable.Typeable effect, Eq effect) =>
  Codec.Codec effect ->
  Codec.Codec (DestructionR.DestructionR effect)
codec effectCodec = Fields.object $ do
  matching <- Fields.defaulted "matching" Nothing (Common.maybe (Filter.codec Keyword.codec)) DestructionR.matching
  rewrite <- Fields.required "rewrite" (DestructionRewrite.codec effectCodec) DestructionR.rewrite
  pure
    DestructionR.MkDestructionR
      { DestructionR.matching = matching,
        DestructionR.rewrite = rewrite
      }
