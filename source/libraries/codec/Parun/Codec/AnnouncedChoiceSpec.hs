module Parun.Codec.AnnouncedChoiceSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Parun.Codec.AnnouncedChoice as AnnouncedChoice
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AnnouncedChoice as AnnouncedChoice
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AnnouncedChoice" $ do
  let codec = Common.assertCodec s AnnouncedChoice.codec
  Spec.it s "Colors" $ codec (AnnouncedChoice.MkColors (Set.fromList [Color.White, Color.Red])) " {\"type\":\"Colors\",\"value\":[\"White\",\"Red\"]} "
  Spec.it s "Colorless" $ codec AnnouncedChoice.MkColorless " {\"type\":\"Colorless\"} "
  Spec.it s "Subtype" $ codec (AnnouncedChoice.MkSubtype Subtype.Merfolk) " {\"type\":\"Subtype\",\"value\":\"Merfolk\"} "
  Spec.it s "CardType" $ codec (AnnouncedChoice.MkCardType CardType.Creature) " {\"type\":\"CardType\",\"value\":\"Creature\"} "
  Spec.it s "Number" $ codec (AnnouncedChoice.MkNumber 3) " {\"type\":\"Number\",\"value\":3} "
  Spec.it s "Parity" $ codec (AnnouncedChoice.MkParity Parity.Odd) " {\"type\":\"Parity\",\"value\":\"Odd\"} "
  Spec.it s "Word" $ codec (AnnouncedChoice.MkWord (Text.pack "e")) " {\"type\":\"Word\",\"value\":\"e\"} "
  Spec.it s "Name" $ codec (AnnouncedChoice.MkName (CardName.MkCardName (Text.pack "Shock"))) " {\"type\":\"Name\",\"value\":\"Shock\"} "
  Spec.it s "Card" $ codec (AnnouncedChoice.MkCard (ObjectId.MkObjectId 7)) " {\"type\":\"Card\",\"value\":7} "
  Spec.it s "Player" $ codec (AnnouncedChoice.MkPlayer (PlayerId.MkPlayerId 1)) " {\"type\":\"Player\",\"value\":1} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s AnnouncedChoice.codec
