{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.StackPlacement where

import qualified Parun.Codec.Label as Label
import qualified Parun.Codec.Placement as Placement
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.StackPlacement as StackPlacement

-- | A placement's keys beside its `owner`'s, in one object.
codec :: Codec.Codec StackPlacement.StackPlacement
codec = Fields.object $ do
  owner <- Fields.required "owner" Label.codec StackPlacement.owner
  placement <- Fields.contramap StackPlacement.placement Placement.fields
  pure
    StackPlacement.MkStackPlacement
      { StackPlacement.owner = owner,
        StackPlacement.placement = placement
      }
