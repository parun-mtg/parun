module Parun.Codec.CompanionCondition where

import qualified Data.Typeable as Typeable
import qualified Parun.Codec.Filter as Filter
import qualified Parun.JsonCodec.Arm as Arm
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Types.CompanionCondition as CompanionCondition

-- | The keyword codec is a PARAMETER; see Parun.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CompanionCondition.CompanionCondition keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "EachCard" (Filter.codec keywordCodec) CompanionCondition.EachCard (\x -> case x of CompanionCondition.EachCard y -> Just y; _ -> Nothing),
      Arm.payload "CardsOverMinimum" Common.natural CompanionCondition.CardsOverMinimum (\x -> case x of CompanionCondition.CardsOverMinimum y -> Just y; _ -> Nothing),
      Arm.payload "DifferentNames" (Filter.codec keywordCodec) CompanionCondition.DifferentNames (\x -> case x of CompanionCondition.DifferentNames y -> Just y; _ -> Nothing)
    ]

tagOf :: CompanionCondition.CompanionCondition keyword -> String
tagOf x = case x of
  CompanionCondition.EachCard {} -> "EachCard"
  CompanionCondition.CardsOverMinimum {} -> "CardsOverMinimum"
  CompanionCondition.DifferentNames {} -> "DifferentNames"
