module Parun.Codec.AnnouncedChoice where

import qualified Parun.Codec.CardName as CardName
import qualified Parun.Codec.CardType as CardType
import qualified Parun.Codec.Color as Color
import qualified Parun.Codec.ObjectId as ObjectId
import qualified Parun.Codec.Parity as Parity
import qualified Parun.Codec.PlayerId as PlayerId
import qualified Parun.Codec.Subtype as Subtype
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.AnnouncedChoice as AnnouncedChoice

-- | Runtime-only, as Parun.Codec.BallotChoice is.
codec :: Codec.Codec AnnouncedChoice.AnnouncedChoice
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Colors" (Common.set Color.codec) AnnouncedChoice.MkColors (\x -> case x of AnnouncedChoice.MkColors y -> Just y; _ -> Nothing),
      Arm.nullary "Colorless" AnnouncedChoice.MkColorless,
      Arm.payload "Subtype" Subtype.codec AnnouncedChoice.MkSubtype (\x -> case x of AnnouncedChoice.MkSubtype y -> Just y; _ -> Nothing),
      Arm.payload "CardType" CardType.codec AnnouncedChoice.MkCardType (\x -> case x of AnnouncedChoice.MkCardType y -> Just y; _ -> Nothing),
      Arm.payload "Number" Common.natural AnnouncedChoice.MkNumber (\x -> case x of AnnouncedChoice.MkNumber y -> Just y; _ -> Nothing),
      Arm.payload "Parity" Parity.codec AnnouncedChoice.MkParity (\x -> case x of AnnouncedChoice.MkParity y -> Just y; _ -> Nothing),
      Arm.payload "Word" Common.text AnnouncedChoice.MkWord (\x -> case x of AnnouncedChoice.MkWord y -> Just y; _ -> Nothing),
      Arm.payload "Name" CardName.codec AnnouncedChoice.MkName (\x -> case x of AnnouncedChoice.MkName y -> Just y; _ -> Nothing),
      Arm.payload "Card" ObjectId.codec AnnouncedChoice.MkCard (\x -> case x of AnnouncedChoice.MkCard y -> Just y; _ -> Nothing),
      Arm.payload "Player" PlayerId.codec AnnouncedChoice.MkPlayer (\x -> case x of AnnouncedChoice.MkPlayer y -> Just y; _ -> Nothing)
    ]

tagOf :: AnnouncedChoice.AnnouncedChoice -> String
tagOf x = case x of
  AnnouncedChoice.MkColors {} -> "Colors"
  AnnouncedChoice.MkColorless -> "Colorless"
  AnnouncedChoice.MkSubtype {} -> "Subtype"
  AnnouncedChoice.MkCardType {} -> "CardType"
  AnnouncedChoice.MkNumber {} -> "Number"
  AnnouncedChoice.MkParity {} -> "Parity"
  AnnouncedChoice.MkWord {} -> "Word"
  AnnouncedChoice.MkName {} -> "Name"
  AnnouncedChoice.MkCard {} -> "Card"
  AnnouncedChoice.MkPlayer {} -> "Player"
