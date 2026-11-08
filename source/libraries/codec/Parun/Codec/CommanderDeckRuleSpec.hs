module Parun.Codec.CommanderDeckRuleSpec where

import qualified Parun.Codec.CommanderDeckRule as CommanderDeckRule
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CommanderDeckRule as CommanderDeckRule

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.CommanderDeckRule" $ do
  Spec.it s "NoMaximumDeckSize" $
    Common.assertCodec s CommanderDeckRule.codec CommanderDeckRule.NoMaximumDeckSize " \"NoMaximumDeckSize\" "
