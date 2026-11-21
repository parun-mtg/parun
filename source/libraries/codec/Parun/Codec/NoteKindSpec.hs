module Parun.Codec.NoteKindSpec where

import qualified Parun.Codec.NoteKind as NoteKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.NoteKind as NoteKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.NoteKind" $ do
  Spec.it s "LifeTotal" $
    Common.assertCodec
      s
      NoteKind.codec
      NoteKind.LifeTotal
      " \"LifeTotal\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s NoteKind.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s NoteKind.codec
