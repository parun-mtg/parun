{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ManaRiderEffect where

import qualified Parun.Codec.Duration as Duration
import qualified Parun.Codec.Keyword as Keyword
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ManaRiderEffect as ManaRiderEffect

-- | A grant's duration is REQUIRED: Generator Servant's "until end of turn"
-- and Domri, Chaos Bringer's unbounded "it gains riot" both print one.
codec :: Codec.Codec ManaRiderEffect.ManaRiderEffect
codec =
  let grant = Fields.object $ do
        keyword <- Fields.required "keyword" Keyword.codec fst
        duration <- Fields.required "duration" Duration.codec snd
        pure (keyword, duration)
   in Arm.tagged
        tagOf
        [ Arm.nullary "CantBeCountered" ManaRiderEffect.CantBeCountered,
          Arm.payload "GainsKeyword" grant (uncurry ManaRiderEffect.GainsKeyword) (\x -> case x of ManaRiderEffect.GainsKeyword y z -> Just (y, z); _ -> Nothing)
        ]

tagOf :: ManaRiderEffect.ManaRiderEffect -> String
tagOf x = case x of
  ManaRiderEffect.CantBeCountered -> "CantBeCountered"
  ManaRiderEffect.GainsKeyword {} -> "GainsKeyword"
