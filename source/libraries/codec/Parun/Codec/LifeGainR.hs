{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.LifeGainR where

import qualified Parun.Codec.ControllerRelation as ControllerRelation
import qualified Parun.Codec.LifeGainCause as LifeGainCause
import qualified Parun.Codec.LifeGainRewrite as LifeGainRewrite
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.LifeGainCause as LifeGainCause
import qualified Parun.Types.LifeGainR as LifeGainR

codec :: Codec.Codec LifeGainR.LifeGainR
codec = Fields.object $ do
  whose <- Fields.required "whose" ControllerRelation.codec LifeGainR.whose
  whichCause <- Fields.defaulted "whichCause" LifeGainCause.AnyCause LifeGainCause.codec LifeGainR.whichCause
  rewrite <- Fields.required "rewrite" LifeGainRewrite.codec LifeGainR.rewrite
  pure
    LifeGainR.MkLifeGainR
      { LifeGainR.whose = whose,
        LifeGainR.whichCause = whichCause,
        LifeGainR.rewrite = rewrite
      }
