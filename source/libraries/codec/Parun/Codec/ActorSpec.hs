module Parun.Codec.ActorSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Actor as Actor
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Actor as Actor
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Actor" $ do
  Spec.it s "Candidate" $
    Common.assertCodec
      s
      Actor.codec
      Actor.Candidate
      " {\"type\":\"Candidate\"} "
  Spec.it s "Source" $
    Common.assertCodec
      s
      Actor.codec
      Actor.Source
      " {\"type\":\"Source\"} "
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      Actor.codec
      (Actor.InSlot (SlotName.MkSlotName (Text.pack "yours")))
      " {\"type\":\"InSlot\",\"value\":\"yours\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Actor.codec
