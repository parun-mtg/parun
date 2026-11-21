module Parun.Codec.TransformTriggerSpec where

import qualified Parun.Codec.TransformTrigger as TransformTrigger
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActingPermanent as ActingPermanent
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Subtype as Subtype
import qualified Parun.Types.TransformTrigger as TransformTrigger

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TransformTrigger" $ do
  Spec.it s "writes no key for the defaults" $
    Common.assertCodec s TransformTrigger.codec TransformTrigger.defaultValue " {} "
  -- Howlpack Piper's "transforms into Wildsong Howler", its own name.
  Spec.it s "round-trips a Self subject into itself" $
    Common.assertCodec
      s
      TransformTrigger.codec
      TransformTrigger.defaultValue {TransformTrigger.intoItself = True}
      " {\"intoItself\":true} "
  -- Cult of the Waxing Moon's bystander Filter. No printable value writes both
  -- keys: intoItself beside a Matching subject is unprinted
  -- (Parun.Types.TransformTrigger), and the case above carries it.
  Spec.it s "round-trips a Matching subject" $
    Common.assertCodec
      s
      TransformTrigger.codec
      TransformTrigger.defaultValue {TransformTrigger.permanent = ActingPermanent.Matching (Filter.Not (Filter.HasSubtype Subtype.Human))}
      " {\"permanent\":{\"type\":\"Matching\",\"value\":{\"type\":\"Not\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Human\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TransformTrigger.codec
