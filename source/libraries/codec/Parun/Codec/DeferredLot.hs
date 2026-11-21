{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.DeferredLot where

import qualified Parun.Codec.AttackChoice as AttackChoice
import qualified Parun.Codec.BlockChoice as BlockChoice
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.DeferredLot as DeferredLot

-- | A lot without riders writes only its tokens.
codec :: Codec.Codec DeferredLot.DeferredLot
codec = Fields.object $ do
  tokens <- Fields.required "tokens" (Common.list ObjectId.codec) DeferredLot.tokens
  attacking <- Fields.defaulted "attacking" Nothing (Common.maybe AttackChoice.codec) DeferredLot.attacking
  blocking <- Fields.defaulted "blocking" Nothing (Common.maybe BlockChoice.codec) DeferredLot.blocking
  pure
    DeferredLot.MkDeferredLot
      { DeferredLot.tokens = tokens,
        DeferredLot.attacking = attacking,
        DeferredLot.blocking = blocking
      }
