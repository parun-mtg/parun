{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ReplacedDestruction where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.DestructionCause as DestructionCause
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ReplacedDestruction as ReplacedDestruction

-- | An object keyed by the record's field names, over the codec of the board
-- it holds. Runtime-only.
codec :: (Typeable.Typeable state) => Codec.Codec state -> Codec.Codec (ReplacedDestruction.ReplacedDestruction state)
codec state = Fields.object $ do
  board <- Fields.required "board" state ReplacedDestruction.board
  cause <- Fields.required "cause" DestructionCause.codec ReplacedDestruction.cause
  pure ReplacedDestruction.MkReplacedDestruction {ReplacedDestruction.board = board, ReplacedDestruction.cause = cause}
