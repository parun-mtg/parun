module Parun.Codec.ManaRSpec where

import qualified Parun.Codec.ManaR as ManaR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.ManaInstead as ManaInstead
import qualified Parun.Types.ManaR as ManaR
import qualified Parun.Types.ManaScale as ManaScale
import qualified Parun.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ManaR" $ do
  -- CR 106.12b: Ritual of Subdual.
  Spec.it s "MkManaR" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) (Just (ManaInstead.OfType ManaType.Colorless)) ManaScale.Keeps Nothing)
      " {\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"produces\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colorless\"}}} "
  -- CR 106.12b's "and/or amount": Contamination.
  Spec.it s "MkManaR with an amount" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) (Just (ManaInstead.OfType (ManaType.Colored Color.Black))) (ManaScale.Exactly 1) Nothing)
      " {\"amount\":{\"type\":\"Exactly\",\"value\":1},\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"produces\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":\"Black\"}}} "
  -- CR 106.12b's "tapped for mana of a specific ... amount": Damping Sphere.
  Spec.it s "MkManaR with a least amount" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) (Just (ManaInstead.OfType ManaType.Colorless)) (ManaScale.Exactly 1) (Just 2))
      " {\"amount\":{\"type\":\"Exactly\",\"value\":1},\"atLeast\":2,\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"},\"produces\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colorless\"}}} "
  -- CR 106.12b: Mana Reflection.
  Spec.it s "MkManaR keeping the type" $
    Common.assertCodec
      s
      ManaR.codec
      (ManaR.MkManaR (Filter.HasCardType CardType.Land) Nothing (ManaScale.Times 2) Nothing)
      " {\"amount\":{\"type\":\"Times\",\"value\":2},\"matching\":{\"type\":\"HasCardType\",\"value\":\"Land\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaR.codec
