{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.OrTapped where

import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.EntryPrice as EntryPrice
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.OrTapped as OrTapped

codec :: Codec.Codec OrTapped.OrTapped
codec = Fields.object $ do
  price <- Fields.required "price" EntryPrice.codec OrTapped.price
  unless_ <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) OrTapped.unless
  pure
    OrTapped.MkOrTapped
      { OrTapped.price = price,
        OrTapped.unless = unless_
      }
