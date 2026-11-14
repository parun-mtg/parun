{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LifeExchange where

import qualified Parun.Codec.ExchangeSides as ExchangeSides
import qualified Parun.Codec.SlotName as SlotName
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LifeExchange as LifeExchange

-- | The sides, and a "tally" elided when absent, Parun.Codec.LifeLoss's posture.
codec :: Codec.Codec LifeExchange.LifeExchange
codec = Fields.object $ do
  sides <- Fields.required "sides" ExchangeSides.codec LifeExchange.sides
  tally <- Fields.defaulted "tally" Nothing (Common.maybe SlotName.codec) LifeExchange.tally
  pure
    LifeExchange.MkLifeExchange
      { LifeExchange.sides = sides,
        LifeExchange.tally = tally
      }
