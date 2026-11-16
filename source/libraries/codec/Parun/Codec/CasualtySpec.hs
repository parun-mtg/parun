module Parun.Codec.CasualtySpec where

import qualified Data.Set as Set
import qualified Parun.Codec.Casualty as Casualty
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Casualty as Casualty
import qualified Parun.Types.Loyalty as Loyalty
import qualified Parun.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Casualty" $ do
  -- CR 702.153a: casualty N with no rider writes the amount alone.
  Spec.it s "MkCasualty" $
    Common.assertCodec s Casualty.codec (Casualty.MkCasualty (Just 2) Set.empty Nothing) " {\"amount\":2} "
  -- CR 107.3 / 707.9b: Ob Nixilis, the Adversary's "Casualty X. The copy isn't
  -- legendary and has starting loyalty X."
  Spec.it s "MkCasualty with X and a rider" $
    Common.assertCodec
      s
      Casualty.codec
      (Casualty.MkCasualty Nothing (Set.singleton Supertype.Legendary) (Just Loyalty.Variable))
      " {\"amount\":null,\"copyLoses\":[\"Legendary\"],\"copyLoyalty\":{\"type\":\"Variable\"}} "
