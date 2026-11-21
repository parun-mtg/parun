module Parun.Codec.NotedSpec where

import qualified Data.Sequence as Seq
import qualified Parun.Codec.Noted as Noted
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ManaType as ManaType
import qualified Parun.Types.Noted as Noted

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Noted" $ do
  Spec.it s "NotedMana" $
    Common.assertCodec
      s
      Noted.codec
      (Noted.NotedMana (Seq.singleton ManaType.Colorless))
      " {\"type\":\"NotedMana\",\"value\":[{\"type\":\"Colorless\"}]} "
  Spec.it s "NotedLifeTotal" $
    Common.assertCodec
      s
      Noted.codec
      (Noted.NotedLifeTotal (-3))
      " {\"type\":\"NotedLifeTotal\",\"value\":-3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Noted.codec
