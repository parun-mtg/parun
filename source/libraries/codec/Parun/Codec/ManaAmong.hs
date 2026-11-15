{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ManaAmong where

import qualified Parun.Codec.ColorSpread as ColorSpread
import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ColorSpread as ColorSpread
import qualified Parun.Types.ManaAmong as ManaAmong

-- | The spread defaults to one choice for every unit, what "{R} or {G}" means.
codec :: Codec.Codec ManaAmong.ManaAmong
codec = Fields.object $ do
  types <- Fields.required "types" (Common.set ManaType.codec) ManaAmong.types
  spread <- Fields.defaulted "spread" ColorSpread.OneColor ColorSpread.codec ManaAmong.spread
  pure ManaAmong.MkManaAmong {ManaAmong.types = types, ManaAmong.spread = spread}
