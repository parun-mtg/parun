module Parun.Types.PutCounterOfEachKind where

import qualified Parun.Types.ObjectRef as ObjectRef

-- | The payload of Parun.Types.Effect's PutCounterOfEachKind arm (CR 122.1,
-- 122.6): Exotic Pets' "for each kind of counter among creatures you control,
-- put a counter of that kind on either of those tokens".
--
-- Not Parun.Types.PutCounters, whose one kind the card settles: here the kinds
-- are read off a group as the instruction is reached (CR 608.2h). Not
-- Parun.Types.PutCountersFrom either, which copies one object's tally (CR
-- 122.8) where this puts ONE counter of each kind.
data PutCounterOfEachKind = MkPutCounterOfEachKind
  { -- | The permanents whose kinds are counted, each kind once however many
    -- members bear it.
    among :: ObjectRef.ObjectRef,
    -- | Where each kind's counter goes, asked once PER KIND (CR 608.2d), so an
    -- ObjectRef.OneAmong is Exotic Pets' "either of those tokens", chosen
    -- kind by kind.
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
