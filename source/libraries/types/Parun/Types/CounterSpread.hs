module Parun.Types.CounterSpread where

-- | How a removal of counters from permanents the payer chooses spreads them
-- (CR 118.1 as a cost, CR 608.2d as an effect): off ONE permanent, or divided
-- among several (CR 601.2h), a fixed count or one the payer settles above a
-- floor or under a ceiling.
data CounterSpread
  = -- | Zameck Guildmage: "Remove a +1\/+1 counter from a creature you control".
    FromOne
  | -- | Novijen Sages: "Remove two +1\/+1 counters from among creatures you
    -- control", the division the payer's own.
    FromAmong
  | -- | Ooze Flux: "Remove one or more +1\/+1 counters from among creatures you
    -- control", the count a floor and how many the payer's own.
    FromAmongAtLeast
  | -- | Mabel, Bitter Recluse: "remove up to three counters from another
    -- target creature", the count a ceiling and how many the remover's own.
    FromAmongAtMost
  deriving (Bounded, Enum, Eq, Ord, Show)
