module Parun.Codec.PlayAllowanceSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.PlayAllowance as PlayAllowance
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.PlayAllowance as PlayAllowance

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayAllowance" $ do
  Spec.it s "March of Reckless Joy's two plays" $
    Common.assertCodec
      s
      PlayAllowance.codec
      (PlayAllowance.Plays 2)
      " {\"type\":\"Plays\",\"value\":2} "
  Spec.it s "Aminatou's Augury's each nonland card type" $
    Common.assertCodec
      s
      PlayAllowance.codec
      (PlayAllowance.EachCardTypeBut (Set.fromList [CardType.Creature, CardType.Land]))
      " {\"type\":\"EachCardTypeBut\",\"value\":[\"Creature\",\"Land\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayAllowance.codec
