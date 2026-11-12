module Parun.Codec.DraftNoteSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.DraftNote as DraftNote
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Color as Color
import qualified Parun.Types.DraftNote as DraftNote
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.DraftNote" $ do
  Spec.it s "Number" $
    Common.assertCodec s DraftNote.codec (DraftNote.Number 4) " {\"type\":\"Number\",\"value\":4} "
  Spec.it s "Color" $
    Common.assertCodec s DraftNote.codec (DraftNote.Color Color.Red) " {\"type\":\"Color\",\"value\":\"Red\"} "
  Spec.it s "Name" $
    Common.assertCodec s DraftNote.codec (DraftNote.Name (CardName.MkCardName (Text.pack "Lightning Bolt"))) " {\"type\":\"Name\",\"value\":\"Lightning Bolt\"} "
  Spec.it s "CreatureType" $
    Common.assertCodec s DraftNote.codec (DraftNote.CreatureType Subtype.Goblin) " {\"type\":\"CreatureType\",\"value\":\"Goblin\"} "
  Spec.it s "Player" $
    Common.assertCodec s DraftNote.codec (DraftNote.Player (PlayerId.MkPlayerId 1)) " {\"type\":\"Player\",\"value\":1} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DraftNote.codec
