module Parun.Codec.ManaSpendingSpec where

import qualified Parun.Codec.ManaSpending as ManaSpending
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ManaFilter as ManaFilter
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.ProductionTag as ProductionTag

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaSpending" $ do
  Spec.it s "AsProduced" $
    Common.assertCodec
      s
      ManaSpending.codec
      ManaSpending.AsProduced
      " \"AsProduced\" "
  Spec.it s "AnyType" $
    Common.assertCodec
      s
      ManaSpending.codec
      ManaSpending.AnyType
      " \"AnyType\" "
  Spec.it s "AnyColor" $
    Common.assertCodec
      s
      ManaSpending.codec
      (ManaSpending.AnyColor ManaFilter.Any)
      " \"AnyColor\" "
  -- Draugr Necromancer's "mana from snow sources".
  Spec.it s "AnyColor, a filter" $
    Common.assertCodec
      s
      ManaSpending.codec
      (ManaSpending.AnyColor (ManaFilter.Produced ProductionTag.Snow))
      " {\"AnyColor\":{\"type\":\"Produced\",\"value\":\"Snow\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ManaSpending.codec
