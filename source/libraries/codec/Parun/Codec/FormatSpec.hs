module Parun.Codec.FormatSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Format as Format
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Format as Format
import qualified Parun.Types.RuleFamily as RuleFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Format" $ do
  Spec.it s "a pool" $
    Common.assertCodec s Format.codec (Format.MkFormat RuleFamily.Constructed (Just (Text.pack "vintage"))) " {\"rules\":\"Constructed\",\"pool\":\"vintage\"} "
  Spec.it s "no pool" $
    Common.assertCodec s Format.codec (Format.MkFormat RuleFamily.Limited Nothing) " {\"rules\":\"Limited\"} "
