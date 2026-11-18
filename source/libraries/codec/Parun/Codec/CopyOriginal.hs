module Parun.Codec.CopyOriginal where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.Types.CopyOriginal as CopyOriginal

codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (CopyOriginal.CopyOriginal card)
codec cardCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "OfObject" ObjectRef.codec CopyOriginal.OfObject (\x -> case x of CopyOriginal.OfObject y -> Just y; _ -> Nothing),
      Arm.payload "Named" CardName.codec CopyOriginal.Named (\x -> case x of CopyOriginal.Named y -> Just y; _ -> Nothing),
      Arm.payload "Defined" cardCodec CopyOriginal.Defined (\x -> case x of CopyOriginal.Defined y -> Just y; _ -> Nothing)
    ]

tagOf :: CopyOriginal.CopyOriginal card -> String
tagOf x = case x of
  CopyOriginal.OfObject {} -> "OfObject"
  CopyOriginal.Named {} -> "Named"
  CopyOriginal.Defined {} -> "Defined"
