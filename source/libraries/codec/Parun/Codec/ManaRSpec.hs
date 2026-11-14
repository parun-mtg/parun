module Parun.Codec.ManaRSpec where

import qualified Parun.Codec.ManaR as ManaR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaR" $ do
  -- CR 106.12b: Ritual of Subdual.
  Spec.it s "MkManaR" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) ManaType.Colorless Nothing)
      " {\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"produces\":{\"type\":\"Colorless\"}} "
  -- CR 106.12b's "and/or amount": Contamination.
  Spec.it s "MkManaR with an amount" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) (ManaType.Colored Color.Black) (Just 1))
      " {\"amount\":1,\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"produces\":{\"type\":\"Colored\",\"value\":\"Black\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaR.codec
