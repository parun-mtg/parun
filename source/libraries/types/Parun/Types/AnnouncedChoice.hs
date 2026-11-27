module Parun.Types.AnnouncedChoice where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Parun.Types.CardName as CardName
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.Parity as Parity
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Subtype as Subtype

-- | What a player chose in the open for an object (CR 608.2d).
data AnnouncedChoice
  = -- | Colours (CR 105.1).
    MkColors (Set.Set Color.Color)
  | -- | Colorless in place of a colour (CR 105.2c).
    MkColorless
  | -- | A subtype (CR 205.3).
    MkSubtype Subtype.Subtype
  | -- | A card type (CR 205.2a).
    MkCardType CardType.CardType
  | -- | A number (CR 107.1).
    MkNumber Natural.Natural
  | -- | Odd or even (CR 107.1).
    MkParity Parity.Parity
  | -- | A word or a letter the card lists (CR 614.12c, 123.6d).
    MkWord Text.Text
  | -- | A card name (CR 201.4).
    MkName CardName.CardName
  | -- | A card among a linked pile (CR 607.2d).
    MkCard ObjectId.ObjectId
  | -- | A player (CR 608.2d).
    MkPlayer PlayerId.PlayerId
  deriving (Eq, Ord, Show)
