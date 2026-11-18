module Parun.Codec.ActiveCopyCastSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ActiveCopyCast as ActiveCopyCast
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActiveCopyCast as ActiveCopyCast
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ExilePlayPermission as ExilePlayPermission
import qualified Parun.Types.Expiry as Expiry
import qualified Parun.Types.ManaSpending as ManaSpending
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PermissionVerb as PermissionVerb
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Timestamp as Timestamp

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ActiveCopyCast" $ do
  -- CR 601.3 / 707.12, Discord, Lord of Disharmony's row. Distinct numbers in
  -- every numeric field, so a swap cannot pass.
  Spec.it s "a permission to cast a copy that copies its ability" $
    Common.assertCodec
      s
      ActiveCopyCast.codec
      ActiveCopyCast.MkActiveCopyCast
        { ActiveCopyCast.timestamp = Timestamp.MkTimestamp 5,
          ActiveCopyCast.printing = PrintingId.MkPrintingId 6,
          ActiveCopyCast.name = CardName.MkCardName (Text.pack "Lightning Bolt"),
          ActiveCopyCast.permission =
            (ExilePlayPermission.defaultValue (PlayerId.MkPlayerId 1) (ObjectId.MkObjectId 2))
              { ExilePlayPermission.expiry = Expiry.AtCleanup,
                ExilePlayPermission.spending = ManaSpending.AnyType,
                ExilePlayPermission.verb = PermissionVerb.Cast
              },
          ActiveCopyCast.ability = ObjectId.MkObjectId 7,
          ActiveCopyCast.recopy = Just (Condition.All [])
        }
      " {\"timestamp\":5,\"printing\":6,\"name\":\"Lightning Bolt\",\"permission\":{\"player\":1,\"source\":2,\"expiry\":{\"type\":\"AtCleanup\"},\"spending\":\"AnyType\",\"alternativeCost\":null,\"condition\":null,\"origin\":\"Granted\",\"verb\":\"Cast\",\"increase\":0,\"increaseUnless\":null,\"landEnters\":\"Untapped\",\"spellEnters\":\"Untapped\",\"budget\":null,\"keywords\":[]},\"ability\":7,\"recopy\":{\"type\":\"All\",\"value\":[]}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActiveCopyCast.codec
