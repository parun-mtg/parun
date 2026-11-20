{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.PutCounterOfEachKind where

import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.PutCounterOfEachKind as PutCounterOfEachKind

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Parun.Codec.Effect's PutCounterOfEachKind arm.
codec :: Codec.Codec PutCounterOfEachKind.PutCounterOfEachKind
codec = Fields.object $ do
  among <- Fields.required "among" ObjectRef.codec PutCounterOfEachKind.among
  ref <- Fields.required "ref" ObjectRef.codec PutCounterOfEachKind.ref
  pure
    PutCounterOfEachKind.MkPutCounterOfEachKind
      { PutCounterOfEachKind.among = among,
        PutCounterOfEachKind.ref = ref
      }
