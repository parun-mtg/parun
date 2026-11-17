module Parun.Codec.FightSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Parun.Codec.Fight as Fight
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Fight as Fight
import qualified Parun.Types.Hop as Hop
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Fight" $ do
  -- Prey Upon's two slots, in the order the object keys sort rather than the
  -- order the card's sentence names them.
  Spec.it s "MkFight, both slots" $
    Common.assertCodec
      s
      Fight.codec
      (Fight.MkFight (SlotName.MkSlotName (Text.pack "mine")) (SlotName.MkSlotName (Text.pack "theirs")) Hop.Itself Nothing False)
      " {\"first\":\"mine\",\"second\":\"theirs\"} "
  -- Novel Nunchaku's "equipped creature fights".
  Spec.it s "MkFight, the first slot's host" $
    Common.assertCodec
      s
      Fight.codec
      (Fight.MkFight (SlotName.MkSlotName (Text.pack "self")) (SlotName.MkSlotName (Text.pack "victim")) Hop.Host Nothing False)
      " {\"first\":\"self\",\"hop\":\"Host\",\"second\":\"victim\"} "
  -- The Last Agni Kai's "is dealt excess damage this way".
  Spec.it s "MkFight, the second creature's excess" $
    Common.assertCodec
      s
      Fight.codec
      (Fight.MkFight (SlotName.MkSlotName (Text.pack "mine")) (SlotName.MkSlotName (Text.pack "theirs")) Hop.Itself (Just (SlotName.MkSlotName (Text.pack "over"))) False)
      " {\"excessTally\":\"over\",\"first\":\"mine\",\"second\":\"theirs\"} "
  -- Ezuri's Predation's "each of those tokens fights a different one of those
  -- creatures".
  Spec.it s "MkFight, two groups paired" $
    Common.assertCodec
      s
      Fight.codec
      (Fight.MkFight (SlotName.MkSlotName (Text.pack "beast")) (SlotName.MkSlotName (Text.pack "creature")) Hop.Itself Nothing True)
      " {\"first\":\"beast\",\"paired\":true,\"second\":\"creature\"} "
  -- Unprinted: a paired fight tallying excess, so refused.
  Spec.it s "rejects a paired tally" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode Fight.codec =<< Common.parse (Text.pack "{\"excessTally\":\"over\",\"first\":\"beast\",\"paired\":true,\"second\":\"creature\"}")))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s Fight.codec
