-- CR 607.2d: reading and writing one of an ability's ordered choices of a
-- quality by its Parun.Types.ChoiceOrdinal.
module Parun.Engine.ChoiceOrdinal where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Parun.Types.ChoiceOrdinal as ChoiceOrdinal

-- | How many choices come before the ordinal's.
earlier :: ChoiceOrdinal.ChoiceOrdinal -> Int
earlier = fromEnum

-- | The choice the ordinal names, Nothing where fewer were made.
chosenAt :: ChoiceOrdinal.ChoiceOrdinal -> NonEmpty.NonEmpty a -> Maybe a
chosenAt ordinal = Maybe.listToMaybe . drop (earlier ordinal) . NonEmpty.toList

-- | The choices before the ordinal's: the options a Distinct choice leaves out.
before :: ChoiceOrdinal.ChoiceOrdinal -> Maybe (NonEmpty.NonEmpty a) -> [a]
before ordinal = take (earlier ordinal) . foldMap NonEmpty.toList

-- | `x` written at the ordinal, replacing the choice there and every later one.
chooseAt :: ChoiceOrdinal.ChoiceOrdinal -> a -> Maybe (NonEmpty.NonEmpty a) -> NonEmpty.NonEmpty a
chooseAt ordinal x old = NonEmpty.prependList (before ordinal old) (NonEmpty.singleton x)
