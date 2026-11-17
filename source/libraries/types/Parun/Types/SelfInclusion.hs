module Parun.Types.SelfInclusion where

-- | Whether a Parun.Types.Anchor.AnyPermanent group may answer with the candidate itself.
data SelfInclusion
  = -- | CR 109.2: any permanent the nest admits, the candidate among them (Seasinger's Island creature controls an Island).
    IncludesCandidate
  | -- | CR 109.2: a permanent other than the candidate, "another permanent you control" (Yenna, Redtooth Regent).
    ExcludesCandidate
  deriving (Bounded, Enum, Eq, Ord, Show)
