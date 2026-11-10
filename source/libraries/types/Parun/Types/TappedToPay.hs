module Parun.Types.TappedToPay where

import qualified Parun.Types.KeywordFamily as KeywordFamily
import qualified Parun.Types.ObjectId as ObjectId

-- | CR 601.2h / 701.26a: one permanent became tapped to pay the cost a keyword
-- ability states, and which keyword's.
data TappedToPay = MkTappedToPay
  { permanent :: ObjectId.ObjectId,
    keyword :: KeywordFamily.KeywordFamily
  }
  deriving (Eq, Ord, Show)
