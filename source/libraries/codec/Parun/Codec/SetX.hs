{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.SetX where

import qualified Parun.Codec.NewX as NewX
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.SetX as SetX

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.Effect's SetX arm.
codec :: Codec.Codec SetX.SetX
codec = Fields.object $ do
  value <- Fields.required "value" NewX.codec SetX.value
  ref <- Fields.required "ref" ObjectRef.codec SetX.ref
  pure
    SetX.MkSetX
      { SetX.value = value,
        SetX.ref = ref
      }
