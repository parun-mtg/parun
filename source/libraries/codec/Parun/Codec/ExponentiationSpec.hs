module Parun.Codec.ExponentiationSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Exponentiation as Exponentiation
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Exponentiation as Exponentiation
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

-- | Instantiated at 'Quantity.Quantity', Parun.Codec.TimesSpec's shape and for
-- its reason.
codec :: Codec.Codec (Exponentiation.Exponentiation Quantity.Quantity)
codec = Exponentiation.codec Quantity.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Exponentiation" $ do
  -- CR 107.1: Mathemagics' 2 to the X.
  Spec.it s "MkExponentiation" $
    Common.assertCodec
      s
      codec
      ( Exponentiation.MkExponentiation
          { Exponentiation.base = Quantity.Literal 2,
            Exponentiation.exponent = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))
          }
      )
      " {\"base\":{\"type\":\"Literal\",\"value\":2},\"exponent\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
