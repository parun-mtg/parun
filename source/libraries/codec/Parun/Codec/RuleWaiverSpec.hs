module Parun.Codec.RuleWaiverSpec where

import qualified Parun.Codec.RuleWaiver as RuleWaiver
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RuleWaiver" $ do
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RuleWaiver.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s RuleWaiver.codec
