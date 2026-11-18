module Parun.Types.ActiveCopyCast where

import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ExilePlayPermission as ExilePlayPermission
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PrintingId as PrintingId
import qualified Parun.Types.Timestamp as Timestamp

-- | One stored Parun.Types.GrantCopyCast permission (CR 601.3): its holder may
-- cast one copy (CR 707.12) of this printing, created outside the game (CR
-- 707.13), under the permission's riders.
data ActiveCopyCast = MkActiveCopyCast
  { -- | Which row this is: the timestamp it was stored at.
    timestamp :: Timestamp.Timestamp,
    printing :: PrintingId.PrintingId,
    -- | The name chosen (CR 201.4): "a spell with that name" is the face of
    -- the copy bearing it.
    name :: CardName.CardName,
    -- | The holder, source, expiry and CR 118.14 rider, carried on the copy
    -- Parun.Engine.Event.offerOutside stands in as an object while its holder
    -- has priority, so the cast reads it as any held permission.
    permission :: ExilePlayPermission.ExilePlayPermission,
    -- | CR 707.10: the ability `recopy` copies, by the id it had on the stack.
    ability :: ObjectId.ObjectId,
    recopy :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
