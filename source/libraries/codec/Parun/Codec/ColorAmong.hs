{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ColorAmong where

import qualified Parun.Codec.ColorSpread as ColorSpread
import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ColorAmong as ColorAmong

-- | No spread means one mana of each colour, no choice in it.
codec :: Codec.Codec ColorAmong.ColorAmong
codec = Fields.object $ do
  among <- Fields.required "among" (Filter.codec Keyword.codec) ColorAmong.among
  spread <- Fields.defaulted "spread" Nothing (Common.maybe ColorSpread.codec) ColorAmong.spread
  pure ColorAmong.MkColorAmong {ColorAmong.among = among, ColorAmong.spread = spread}
