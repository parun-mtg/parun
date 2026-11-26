module Parun.Codec.NamedCopySource where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.FromReference as FromReference
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.NamedCopySource as NamedCopySource

codec :: Codec.Codec NamedCopySource.NamedCopySource
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Unchosen" (Common.nonEmpty CardName.codec) NamedCopySource.Unchosen (\x -> case x of NamedCopySource.Unchosen y -> Just y; _ -> Nothing),
      Arm.payload "AtRandom" FromReference.codec NamedCopySource.AtRandom (\x -> case x of NamedCopySource.AtRandom y -> Just y; _ -> Nothing)
    ]

tagOf :: NamedCopySource.NamedCopySource -> String
tagOf x = case x of
  NamedCopySource.Unchosen {} -> "Unchosen"
  NamedCopySource.AtRandom {} -> "AtRandom"
