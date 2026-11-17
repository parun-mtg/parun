module Parun.Codec.ProductionTag where

import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.ProductionTag as ProductionTag

-- | Keyed, so a nullary tag stays the bare string 'Arm.enum' wrote before
-- Subtype grew a payload.
codec :: Codec.Codec ProductionTag.ProductionTag
codec =
  Arm.keyed
    tagOf
    [ Arm.nullary "Snow" ProductionTag.Snow,
      Arm.payload "Subtype" Subtype.codec ProductionTag.Subtype (\x -> case x of ProductionTag.Subtype y -> Just y; _ -> Nothing),
      Arm.nullary "Artifact" ProductionTag.Artifact
    ]

tagOf :: ProductionTag.ProductionTag -> String
tagOf x = case x of
  ProductionTag.Snow -> "Snow"
  ProductionTag.Subtype {} -> "Subtype"
  ProductionTag.Artifact -> "Artifact"
