module Parun.Codec.ModeSelectionSpec where

import qualified Data.Either as Either
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.ModeSelection as ModeSelection
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ChooseBetween as ChooseBetween
import qualified Parun.Types.ChooseInstead as ChooseInstead
import qualified Parun.Types.ChoosePawprints as ChoosePawprints
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ModeSelection" $ do
  Spec.it s "ChooseExactly" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseExactly 1)
      " {\"type\":\"ChooseExactly\",\"value\":1} "

  -- CR 700.2d's exception. The encoding of the default above is byte-identical to
  -- what it was before this constructor existed, which is what let every card
  -- file stay as written.
  Spec.it s "ChooseExactlyWithRepeats" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseExactlyWithRepeats 3)
      " {\"type\":\"ChooseExactlyWithRepeats\",\"value\":3} "

  -- CR 700.2's "Choose one or both --" (data/cards/vandalize.json). Named fields
  -- rather than a two-element list, so the bound a card means is written down.
  Spec.it s "ChooseBetween" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseBetween (ChooseBetween.MkChooseBetween 1 2))
      " {\"type\":\"ChooseBetween\",\"value\":{\"least\":1,\"most\":2}} "

  -- CR 700.2d's exception over a range (data/cards/moment-of-reckoning.json).
  Spec.it s "ChooseBetweenWithRepeats" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseBetweenWithRepeats (ChooseBetween.MkChooseBetween 0 4))
      " {\"type\":\"ChooseBetweenWithRepeats\",\"value\":{\"least\":0,\"most\":4}} "

  -- Widow's Bite (data/cards/widows-bite.json), nesting the tag's own codec.
  Spec.it s "ChooseInstead" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseInstead (ChooseInstead.MkChooseInstead (Condition.Compares (Compares.MkCompares (Quantity.Literal 1) Comparison.AtLeast (Quantity.Literal 0))) (ModeSelection.ChooseExactly 1) (ModeSelection.ChooseExactly 2)))
      " {\"type\":\"ChooseInstead\",\"value\":{\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":1},\"comparison\":\"AtLeast\",\"threshold\":{\"type\":\"Literal\",\"value\":0}}},\"ordinary\":{\"type\":\"ChooseExactly\",\"value\":1},\"instead\":{\"type\":\"ChooseExactly\",\"value\":2}}} "

  -- The Ruinous Wrecking Crew (data/cards/the-ruinous-wrecking-crew.json).
  Spec.it s "ChooseUpTo" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChooseUpTo (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))))
      " {\"type\":\"ChooseUpTo\",\"value\":{\"type\":\"InSlot\",\"value\":\"X\"}} "

  -- CR 700.2i (data/cards/season-of-loss.json).
  Spec.it s "ChoosePawprintsWithRepeats" $
    Common.assertCodec
      s
      ModeSelection.codec
      (ModeSelection.ChoosePawprintsWithRepeats (ChoosePawprints.MkChoosePawprints 5 (Seq.fromList [1, 2, 3])))
      " {\"type\":\"ChoosePawprintsWithRepeats\",\"value\":{\"most\":5,\"pawprints\":[1,2,3]}} "

  Spec.it s "has a schema" $ Common.assertHasSchema s ModeSelection.codec

  -- The one invariant Parun.Types.ModeSelection states, enforced by
  -- Parun.Codec.ChooseBetween and reached through this tag.
  Spec.it s "rejects a minimum above the maximum" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"ChooseBetween\",\"value\":{\"least\":2,\"most\":1}} ") >>= Codec.decode ModeSelection.codec))
      "expected a decode failure"
