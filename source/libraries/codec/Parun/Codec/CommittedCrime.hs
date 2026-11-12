{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.CommittedCrime where

import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.CommittedCrime as CommittedCrime

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec CommittedCrime.CommittedCrime
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec CommittedCrime.player
  object <- Fields.required "object" ObjectId.codec CommittedCrime.object
  pure
    CommittedCrime.MkCommittedCrime
      { CommittedCrime.player = player,
        CommittedCrime.object = object
      }
