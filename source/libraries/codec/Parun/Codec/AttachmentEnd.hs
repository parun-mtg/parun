module Parun.Codec.AttachmentEnd where

import qualified Parun.Codec.Filter as Filter
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.AttachmentEnd as AttachmentEnd

codec :: Codec.Codec AttachmentEnd.AttachmentEnd
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Host" AttachmentEnd.Host,
      Arm.nullary "Attachment" AttachmentEnd.Attachment,
      Arm.payload "Neither" (Filter.codec Keyword.codec) AttachmentEnd.Neither (\x -> case x of AttachmentEnd.Neither y -> Just y; _ -> Nothing)
    ]

tagOf :: AttachmentEnd.AttachmentEnd -> String
tagOf x = case x of
  AttachmentEnd.Host {} -> "Host"
  AttachmentEnd.Attachment {} -> "Attachment"
  AttachmentEnd.Neither {} -> "Neither"
