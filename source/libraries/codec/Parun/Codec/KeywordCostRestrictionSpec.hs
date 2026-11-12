module Parun.Codec.KeywordCostRestrictionSpec where

import qualified Parun.Codec.KeywordCostRestriction as KeywordCostRestriction
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Affected as Affected
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.KeywordCostPart as KeywordCostPart
import qualified Parun.Types.KeywordCostRestriction as KeywordCostRestriction

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.KeywordCostRestriction" $ do
  -- Revoke Privileges' third clause (CR 702.122d / CR 101.2), written the way
  -- its own first two clauses are: an Aura's "enchanted creature" is
  -- HasAttached IsSource, Pacifism's spelling.
  Spec.it s "MkKeywordCostRestriction Crew" $
    Common.assertCodec
      s
      KeywordCostRestriction.codec
      ( KeywordCostRestriction.MkKeywordCostRestriction
          KeywordCostPart.Crew
          ( Affected.Matching
              ( Filter.And
                  [ Filter.HasCardType CardType.Creature,
                    Filter.HasAttached Filter.IsSource
                  ]
              )
          )
      )
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":\"Creature\"},{\"type\":\"HasAttached\",\"value\":{\"type\":\"IsSource\"}}]}},\"part\":\"Crew\"} "
  -- Marooned's "can't be beamed up" (CR 702.196b).
  Spec.it s "MkKeywordCostRestriction BeamMeUp" $
    Common.assertCodec
      s
      KeywordCostRestriction.codec
      (KeywordCostRestriction.MkKeywordCostRestriction KeywordCostPart.BeamMeUp (Affected.Matching (Filter.HasAttached Filter.IsSource)))
      " {\"affected\":{\"type\":\"Matching\",\"value\":{\"type\":\"HasAttached\",\"value\":{\"type\":\"IsSource\"}}},\"part\":\"BeamMeUp\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s KeywordCostRestriction.codec
