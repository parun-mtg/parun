module Parun.Codec.RecipientKindSpec where

import qualified Parun.Codec.RecipientKind as RecipientKind
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.RecipientKind as RecipientKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RecipientKind" $ do
  Spec.it s "AnyRecipient" $
    Common.assertCodec
      s
      RecipientKind.codec
      RecipientKind.AnyRecipient
      " \"AnyRecipient\" "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s RecipientKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s RecipientKind.codec
