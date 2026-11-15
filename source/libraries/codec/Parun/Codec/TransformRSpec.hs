module Parun.Codec.TransformRSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.TransformR as TransformR
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.TransformR as TransformR

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.TransformR" $ do
  Spec.it s "MkTransformR (Sephiroth, One-Winged Angel)" $
    Common.assertCodec
      s
      (TransformR.codec Common.text)
      ( TransformR.MkTransformR
          { TransformR.matching = Filter.IsSource,
            TransformR.effects = Seq.singleton (Text.pack "emblem")
          }
      )
      " {\"matching\":{\"type\":\"IsSource\"},\"effects\":[\"emblem\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (TransformR.codec Common.text)
