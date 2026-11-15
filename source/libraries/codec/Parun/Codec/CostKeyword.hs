module Parun.Codec.CostKeyword where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.Codec.ManaCost as ManaCost
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.CostKeyword as CostKeyword

-- | 'Arm.keyed', so a nullary keyword stays the bare tag the former 'Arm.enum'
-- wrote, and a bare @"BeamMeUp"@ is the grant restricting no creature.
codec :: Codec.Codec CostKeyword.CostKeyword
codec =
  Arm.keyed
    tagOf
    [ Arm.nullary "Flashback" CostKeyword.Flashback,
      Arm.nullary "Scavenge" CostKeyword.Scavenge,
      Arm.nullary "Unearth" CostKeyword.Unearth,
      Arm.nullary "Embalm" CostKeyword.Embalm,
      Arm.nullary "Encore" CostKeyword.Encore,
      Arm.nullary "Disguise" CostKeyword.Disguise,
      Arm.nullary "Blitz" CostKeyword.Blitz,
      Arm.nullary "Harmonize" CostKeyword.Harmonize,
      Arm.nullary "Mayhem" CostKeyword.Mayhem,
      Arm.nullary "Replicate" CostKeyword.Replicate,
      Arm.nullary "Emerge" CostKeyword.Emerge,
      Arm.payload "Miracle" ManaCost.codec CostKeyword.Miracle (\x -> case x of CostKeyword.Miracle y -> Just y; _ -> Nothing),
      Arm.payload "Escape" Common.natural CostKeyword.Escape (\x -> case x of CostKeyword.Escape y -> Just y; _ -> Nothing),
      Arm.optionalPayload "BeamMeUp" (Filter.codec Keyword.codec) CostKeyword.BeamMeUp (\x -> case x of CostKeyword.BeamMeUp y -> Just y; _ -> Nothing)
    ]

tagOf :: CostKeyword.CostKeyword -> String
tagOf x = case x of
  CostKeyword.Flashback -> "Flashback"
  CostKeyword.Scavenge -> "Scavenge"
  CostKeyword.Unearth -> "Unearth"
  CostKeyword.Embalm -> "Embalm"
  CostKeyword.Encore -> "Encore"
  CostKeyword.Disguise -> "Disguise"
  CostKeyword.Blitz -> "Blitz"
  CostKeyword.Harmonize -> "Harmonize"
  CostKeyword.Mayhem -> "Mayhem"
  CostKeyword.Replicate -> "Replicate"
  CostKeyword.Emerge -> "Emerge"
  CostKeyword.Miracle {} -> "Miracle"
  CostKeyword.Escape {} -> "Escape"
  CostKeyword.BeamMeUp {} -> "BeamMeUp"
