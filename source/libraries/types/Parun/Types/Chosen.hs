{-# LANGUAGE GADTs #-}

module Parun.Types.Chosen where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.PlayerChoice as PlayerChoice
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

-- | Which of an object's per-acquisition choices (CR 607.2d / 607.5), indexed
-- by what one acquisition's entry holds: the field Parun.Engine.Game reads and
-- writes on the live object and on its last known information alike.
data Chosen a where
  -- | CR 105.1: the chosen colours (Object.chosenColors).
  Colors :: Chosen (Set.Set Color.Color)
  -- | CR 105.2c: colorless chosen instead of a colour (Object.chosenColorless).
  Colorless :: Chosen ()
  -- | CR 205.3: the chosen subtypes, in the order chosen (Object.chosenSubtype).
  Subtype :: Chosen (NonEmpty.NonEmpty Subtype.Subtype)
  -- | CR 205.2a: the chosen card type (Object.chosenCardType).
  CardType :: Chosen CardType.CardType
  -- | CR 201.4 / 702.106f: the chosen card names, by chooser (Object.chosenNames).
  Names :: Chosen (Map.Map PlayerId.PlayerId (Set.Set CardName.CardName))
  -- | CR 614.1c / 702.174a: the chosen players, by choice (Object.chosenPlayers).
  Players :: Chosen (Map.Map PlayerChoice.PlayerChoice PlayerId.PlayerId)
  -- | CR 607.2d / 607.2g: the last chosen number, or the life paid as it entered or with it (Object.chosenNumber).
  Number :: Chosen Natural.Natural
  -- | CR 607.2d: the last chosen objects (Object.chosenObjects).
  Objects :: Chosen (Set.Set ObjectId.ObjectId)
