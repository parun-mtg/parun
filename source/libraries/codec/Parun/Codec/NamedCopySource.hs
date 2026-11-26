module Parun.Codec.NamedCopySource where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.FromReference as FromReference
import qualified Parun.Codec.ObjectRef as ObjectRef
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.FromReference as FromReference.Type
import qualified Parun.Types.NamedCopySource as NamedCopySource
import qualified Parun.Types.ObjectRef as ObjectRef.Type

codec :: Codec.Codec NamedCopySource.NamedCopySource
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Unchosen" (Common.nonEmpty CardName.codec) NamedCopySource.Unchosen (\x -> case x of NamedCopySource.Unchosen y -> Just y; _ -> Nothing),
      Arm.payload "AtRandom" FromReference.codec NamedCopySource.AtRandom (\x -> case x of NamedCopySource.AtRandom y -> Just y; _ -> Nothing),
      Arm.payload "SameManaCostAs" sameManaCostAs (uncurry NamedCopySource.SameManaCostAs) (\x -> case x of NamedCopySource.SameManaCostAs y z -> Just (y, z); _ -> Nothing)
    ]

-- | The ref and the reference pick, under a key each.
sameManaCostAs :: Codec.Codec (ObjectRef.Type.ObjectRef, FromReference.Type.FromReference)
sameManaCostAs =
  Fields.object $
    (,)
      <$> Fields.required "objects" ObjectRef.codec fst
      <*> Fields.required "from" FromReference.codec snd

tagOf :: NamedCopySource.NamedCopySource -> String
tagOf x = case x of
  NamedCopySource.Unchosen {} -> "Unchosen"
  NamedCopySource.AtRandom {} -> "AtRandom"
  NamedCopySource.SameManaCostAs {} -> "SameManaCostAs"
