module Parun.Codec.SkipWindow where

import qualified Parun.Codec.PhaseSelector as PhaseSelector.Codec
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import Parun.Types.PhaseSelector (PhaseSelector)
import qualified Parun.Types.SkipWindow as SkipWindow
import qualified Parun.Types.TurnKind as TurnKind

-- | Flat: a step or phase keeps Parun.Codec.PhaseSelector's spelling, and each
-- kind of whole turn is one more tag beside them.
codec :: Codec.Codec SkipWindow.SkipWindow
codec =
  Arm.tagged
    tagOf
    ( PhaseSelector.Codec.arms SkipWindow.PartOfTurn partOfTurn
        <> [ Arm.nullary "Turn" (SkipWindow.WholeTurn TurnKind.AnyTurn),
             Arm.nullary "ExtraTurn" (SkipWindow.WholeTurn TurnKind.ExtraTurn)
           ]
    )

partOfTurn :: SkipWindow.SkipWindow -> Maybe PhaseSelector
partOfTurn x = case x of
  SkipWindow.PartOfTurn y -> Just y
  SkipWindow.WholeTurn _ -> Nothing

tagOf :: SkipWindow.SkipWindow -> String
tagOf x = case x of
  SkipWindow.PartOfTurn y -> PhaseSelector.Codec.tagOf y
  SkipWindow.WholeTurn TurnKind.AnyTurn -> "Turn"
  SkipWindow.WholeTurn TurnKind.ExtraTurn -> "ExtraTurn"
