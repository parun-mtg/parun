module Parun.Codec.ProhibitionSpec where

import qualified Parun.Codec.Prohibition as Prohibition
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Prohibition as Prohibition
import qualified Parun.Types.UntapScope as UntapScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Prohibition" $ do
  Spec.it s "Activate" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.Activate
      " {\"type\":\"Activate\"} "
  Spec.it s "Block" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.Block
      " {\"type\":\"Block\"} "
  Spec.it s "Counters" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.Counters
      " {\"type\":\"Counters\"} "
  Spec.it s "PhaseIn" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.PhaseIn
      " {\"type\":\"PhaseIn\"} "
  Spec.it s "Regenerate" $
    Common.assertCodec
      s
      Prohibition.codec
      Prohibition.Regenerate
      " {\"type\":\"Regenerate\"} "
  Spec.it s "Untap" $
    Common.assertCodec
      s
      Prohibition.codec
      (Prohibition.Untap UntapScope.AnyUntap)
      " {\"type\":\"Untap\",\"value\":\"AnyUntap\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Prohibition.codec
