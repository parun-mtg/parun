module Parun.Types.ChosenQuality where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Parun.Types.ChooseBetween as ChooseBetween
import qualified Parun.Types.SubtypeFamily as SubtypeFamily

-- | What a choice of a quality has its chooser pick, written to the object it
-- is made for: as it enters (CR 614.1c, EntryRewrite.ChooseQuality), as it
-- becomes attached (CR 607.2d, AttachRewrite.ChooseQuality) or as it resolves
-- (CR 608.2d, Effect.ChooseQuality).
data ChosenQuality
  = -- | CR 105.4: between least and most different colours, the chooser picking how many, written to Object.chosenColors (Painter's Servant, Scuttlemutt).
    Colors ChooseBetween.ChooseBetween
  | -- | CR 205.3: one subtype of the family, written to Object.chosenSubtype (Convincing Mirage, Pillar of Origins, Deification).
    Subtype SubtypeFamily.SubtypeFamily
  | -- | CR 205.2a: one card type, written to Object.chosenCardType (Umori, the Collector).
    CardType
  | -- | CR 607.2d: one number in the range, replacing the last, written to Object.chosenNumber (Shapeshifter).
    Number ChooseBetween.ChooseBetween
  | -- | CR 607.2d: odd or even, written to Object.chosenNumber as 0 or 1 for Filter.ManaValueParity to read (Gollum, Riddle Master).
    Parity
  | -- | CR 607.2f / 614.12c: one of the card's words, written to Object.chosenNumber as its 1-based position for a Quantity.ChosenNumber condition to read (Citadel Siege).
    Word (NonEmpty.NonEmpty Text.Text)
  deriving (Eq, Ord, Show)
