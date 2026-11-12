module Parun.Codec.RequiredDefenderSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.RequiredDefender as RequiredDefender
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.RequiredDefender as RequiredDefender

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.RequiredDefender" $ do
  Spec.it s "ControllerOfAttached" $
    Common.assertCodec
      s
      RequiredDefender.codec
      RequiredDefender.ControllerOfAttached
      " {\"type\":\"ControllerOfAttached\"} "
  Spec.it s "OpponentWithMostLife" $
    Common.assertCodec
      s
      RequiredDefender.codec
      RequiredDefender.OpponentWithMostLife
      " {\"type\":\"OpponentWithMostLife\"} "
  Spec.it s "ControllerOrTheirPlaneswalkers" $
    Common.assertCodec
      s
      RequiredDefender.codec
      RequiredDefender.ControllerOrTheirPlaneswalkers
      " {\"type\":\"ControllerOrTheirPlaneswalkers\"} "
  Spec.it s "Noted" $
    Common.assertCodec
      s
      RequiredDefender.codec
      (RequiredDefender.Noted (CardName.MkCardName (Text.pack "Cogwork Tracker")))
      " {\"type\":\"Noted\",\"value\":\"Cogwork Tracker\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s RequiredDefender.codec
