module Parun.Codec.CloakSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.Cloak as Cloak
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Cloak as Cloak
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.Cloak" $ do
  -- Ransom Note's "cloak the top card of your library": the library key is
  -- omitted, each instructed player cloaking from their own.
  Spec.it s "MkCloak from the instructed player's own library omits the key" $
    Common.assertCodec
      s
      Cloak.codec
      (Cloak.MkCloak (PlayerRef.Relative PlayerRelation.You) Nothing)
      " {\"player\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Etrata, Deadly Fugitive's "cloak the top card of that player's library".
  Spec.it s "MkCloak from another player's library" $
    Common.assertCodec
      s
      Cloak.codec
      (Cloak.MkCloak (PlayerRef.Relative PlayerRelation.You) (Just (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")))))
      " {\"library\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"player\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Cloak.codec
