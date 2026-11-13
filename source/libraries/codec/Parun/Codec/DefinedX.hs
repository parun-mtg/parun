{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DefinedX where

import qualified Parun.Codec.ExiledPile as ExiledPile
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DefinedX as DefinedX

-- | The choice is emitted only for an X a choice defines (CR 601.2b).
codec :: Codec.Codec DefinedX.DefinedX
codec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec DefinedX.quantity
  choice <- Fields.defaulted "choice" Nothing (Common.maybe ExiledPile.codec) DefinedX.choice
  pure DefinedX.MkDefinedX {DefinedX.quantity = quantity, DefinedX.choice = choice}
