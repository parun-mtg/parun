module Parun.Codec.AttachmentEndSpec where

import qualified Parun.Codec.AttachmentEnd as AttachmentEnd
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttachmentEnd as AttachmentEnd
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttachmentEnd" $ do
  Spec.it s "Host" $
    Common.assertCodec s AttachmentEnd.codec AttachmentEnd.Host " {\"type\":\"Host\"} "
  Spec.it s "Attachment" $
    Common.assertCodec s AttachmentEnd.codec AttachmentEnd.Attachment " {\"type\":\"Attachment\"} "
  Spec.it s "Neither" $
    Common.assertCodec
      s
      AttachmentEnd.codec
      (AttachmentEnd.Neither (Filter.HasSubtype Subtype.Aura))
      " {\"type\":\"Neither\",\"value\":{\"type\":\"HasSubtype\",\"value\":\"Aura\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttachmentEnd.codec
