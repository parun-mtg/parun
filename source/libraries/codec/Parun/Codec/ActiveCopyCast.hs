{-# LANGUAGE ApplicativeDo #-}

module Parun.Codec.ActiveCopyCast where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.Condition as Condition
import qualified Parun.Codec.ExilePlayPermission as ExilePlayPermission
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.PrintingId as PrintingId
import qualified Parun.Codec.Timestamp as Timestamp
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonCodec.Fields as Fields
import qualified Parun.Types.ActiveCopyCast as ActiveCopyCast

-- | Every field 'Fields.required' but @recopy@, a game state's record of a row
-- already stored.
codec :: Codec.Codec ActiveCopyCast.ActiveCopyCast
codec = Fields.object $ do
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveCopyCast.timestamp
  printing <- Fields.required "printing" PrintingId.codec ActiveCopyCast.printing
  name <- Fields.required "name" CardName.codec ActiveCopyCast.name
  permission <- Fields.required "permission" ExilePlayPermission.codec ActiveCopyCast.permission
  ability <- Fields.required "ability" ObjectId.codec ActiveCopyCast.ability
  recopy <- Fields.defaulted "recopy" Nothing (Common.maybe Condition.codec) ActiveCopyCast.recopy
  pure
    ActiveCopyCast.MkActiveCopyCast
      { ActiveCopyCast.timestamp = timestamp,
        ActiveCopyCast.printing = printing,
        ActiveCopyCast.name = name,
        ActiveCopyCast.permission = permission,
        ActiveCopyCast.ability = ability,
        ActiveCopyCast.recopy = recopy
      }
