module Parun.Codec.ModeSelection where

import qualified Parun.Codec.ChooseBetween as ChooseBetween
import qualified Parun.Codec.ChooseInstead as ChooseInstead
import qualified Parun.Codec.Quantity as Quantity
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.ModeSelection as ModeSelection

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema. 'ChooseBetween' already wrote a named object, so giving it a
-- record left every card file alone.
codec :: Codec.Codec ModeSelection.ModeSelection
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "ChooseExactly" Common.natural ModeSelection.ChooseExactly (\x -> case x of ModeSelection.ChooseExactly y -> Just y; _ -> Nothing),
      -- CR 700.2d: "You may choose the same mode more than once." A separate tag
      -- rather than a field on the one above, so a card printing the ordinary
      -- instruction encodes exactly as it always did.
      Arm.payload "ChooseExactlyWithRepeats" Common.natural ModeSelection.ChooseExactlyWithRepeats (\x -> case x of ModeSelection.ChooseExactlyWithRepeats y -> Just y; _ -> Nothing),
      Arm.payload "ChooseBetween" ChooseBetween.codec ModeSelection.ChooseBetween (\x -> case x of ModeSelection.ChooseBetween y -> Just y; _ -> Nothing),
      -- CR 700.2d over a range, separate for ChooseExactlyWithRepeats' reason.
      Arm.payload "ChooseBetweenWithRepeats" ChooseBetween.codec ModeSelection.ChooseBetweenWithRepeats (\x -> case x of ModeSelection.ChooseBetweenWithRepeats y -> Just y; _ -> Nothing),
      -- Recurses on 'codec' itself, Parun.Codec.Condition's reason.
      Arm.payload "ChooseInstead" (ChooseInstead.codec codec) ModeSelection.ChooseInstead (\x -> case x of ModeSelection.ChooseInstead y -> Just y; _ -> Nothing),
      Arm.payload "ChooseUpTo" Quantity.codec ModeSelection.ChooseUpTo (\x -> case x of ModeSelection.ChooseUpTo y -> Just y; _ -> Nothing)
    ]

tagOf :: ModeSelection.ModeSelection -> String
tagOf x = case x of
  ModeSelection.ChooseExactly {} -> "ChooseExactly"
  ModeSelection.ChooseExactlyWithRepeats {} -> "ChooseExactlyWithRepeats"
  ModeSelection.ChooseBetween {} -> "ChooseBetween"
  ModeSelection.ChooseBetweenWithRepeats {} -> "ChooseBetweenWithRepeats"
  ModeSelection.ChooseInstead {} -> "ChooseInstead"
  ModeSelection.ChooseUpTo {} -> "ChooseUpTo"
