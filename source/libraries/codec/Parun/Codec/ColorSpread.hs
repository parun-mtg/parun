module Parun.Codec.ColorSpread where

import qualified Data.Maybe as Maybe
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ColorSpread as ColorSpread

-- | Externally tagged, so a bare tag is a string as it was while the type was
-- all nullary; @DifferentColors@'s run length is optional, its absence runs of
-- one, which is what "of different colors" means.
codec :: Codec.Codec ColorSpread.ColorSpread
codec =
  Arm.keyed
    tagOf
    [ Arm.nullary "OneColor" ColorSpread.OneColor,
      Arm.nullary "AnyCombination" ColorSpread.AnyCombination,
      Arm.optionalPayload "DifferentColors" Common.natural (ColorSpread.DifferentColors . Maybe.fromMaybe 1) (\x -> case x of ColorSpread.DifferentColors 1 -> Just Nothing; ColorSpread.DifferentColors n -> Just (Just n); _ -> Nothing)
    ]

tagOf :: ColorSpread.ColorSpread -> String
tagOf x = case x of
  ColorSpread.OneColor -> "OneColor"
  ColorSpread.AnyCombination -> "AnyCombination"
  ColorSpread.DifferentColors {} -> "DifferentColors"
