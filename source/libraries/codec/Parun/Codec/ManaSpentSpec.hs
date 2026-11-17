module Parun.Codec.ManaSpentSpec where

import qualified Parun.Codec.ManaSpent as ManaSpent
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Color as Color
import qualified Parun.Types.CostPortion as CostPortion
import qualified Parun.Types.ManaSpent as ManaSpent
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.ProductionTag as ProductionTag
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaSpent" $ do
  Spec.it s "Total" $
    Common.assertCodec s ManaSpent.codec ManaSpent.Total " {\"type\":\"Total\"} "
  Spec.it s "OfType" $
    Common.assertCodec
      s
      ManaSpent.codec
      (ManaSpent.OfType (ManaType.Colored Color.Black) CostPortion.OnX)
      " {\"type\":\"OfType\",\"value\":{\"manaType\":{\"type\":\"Colored\",\"value\":\"Black\"},\"portion\":\"OnX\"}} "
  Spec.it s "Colors" $
    Common.assertCodec s ManaSpent.codec ManaSpent.Colors " {\"type\":\"Colors\"} "
  Spec.it s "Tagged" $
    Common.assertCodec s ManaSpent.codec (ManaSpent.Tagged (ProductionTag.Subtype Subtype.Cave)) " {\"type\":\"Tagged\",\"value\":{\"Subtype\":\"Cave\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaSpent.codec
