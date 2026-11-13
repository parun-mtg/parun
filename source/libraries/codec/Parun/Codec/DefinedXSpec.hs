module Parun.Codec.DefinedXSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.DefinedX as DefinedX
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.DefinedX as DefinedX
import qualified Parun.Types.ExiledPile as ExiledPile
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DefinedX" $ do
  Spec.it s "no choice, the default" $
    Common.assertCodec s DefinedX.codec (DefinedX.MkDefinedX (Quantity.Literal 2) Nothing) " {\"quantity\":{\"type\":\"Literal\",\"value\":2}} "
  Spec.it s "a choice out of a pile" $
    Common.assertCodec
      s
      DefinedX.codec
      (DefinedX.MkDefinedX (Quantity.Literal 2) (Just (ExiledPile.WithCardsNamed (CardName.MkCardName (Text.pack "Caller of the Untamed")))))
      " {\"quantity\":{\"type\":\"Literal\",\"value\":2},\"choice\":{\"type\":\"WithCardsNamed\",\"value\":\"Caller of the Untamed\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DefinedX.codec
