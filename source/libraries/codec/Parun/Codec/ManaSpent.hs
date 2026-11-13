module Parun.Codec.ManaSpent where

import qualified Parun.Codec.CostPortion as CostPortion
import qualified Parun.Codec.ManaType as ManaType
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CostPortion as CostPortion.Type
import qualified Parun.Types.ManaSpent as ManaSpent
import qualified Parun.Types.ManaType as ManaType.Type

codec :: Codec.Codec ManaSpent.ManaSpent
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Total" ManaSpent.Total,
      Arm.payload "OfType" ofTypeCodec (uncurry ManaSpent.OfType) (\x -> case x of ManaSpent.OfType y z -> Just (y, z); _ -> Nothing),
      Arm.nullary "Colors" ManaSpent.Colors
    ]

-- OfType's payload: which type of mana, spent on which portion of the cost.
ofTypeCodec :: Codec.Codec (ManaType.Type.ManaType, CostPortion.Type.CostPortion)
ofTypeCodec =
  Fields.object $
    (,)
      <$> Fields.required "manaType" ManaType.codec fst
      <*> Fields.required "portion" CostPortion.codec snd

tagOf :: ManaSpent.ManaSpent -> String
tagOf x = case x of
  ManaSpent.Total {} -> "Total"
  ManaSpent.OfType {} -> "OfType"
  ManaSpent.Colors {} -> "Colors"
