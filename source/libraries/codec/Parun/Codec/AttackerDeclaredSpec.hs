module Parun.Codec.AttackerDeclaredSpec where

import qualified Data.Set as Set
import qualified Parun.Codec.AttackerDeclared as AttackerDeclared
import qualified Parun.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.AttackTarget as AttackTarget
import qualified Parun.Types.AttackerDeclared as AttackerDeclared
import qualified Parun.Types.ObjectId as ObjectId
import qualified Parun.Types.ObjectSnapshot as ObjectSnapshot
import qualified Parun.Types.PlayerId as PlayerId
import qualified Parun.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.AttackerDeclared" $ do
  -- CR 508.1, carrying CR 508.1b's announcement and CR 702.83b's declaration
  -- size. The battle attacked is not the defending player's own id, which is CR
  -- 310.9d's substitution and the reason both fields are on the wire.
  Spec.it s "MkAttackerDeclared, every key" $
    Common.assertCodec
      s
      AttackerDeclared.codec
      ( AttackerDeclared.MkAttackerDeclared
          { AttackerDeclared.attacker = ObjectId.MkObjectId 1,
            AttackerDeclared.defender = PlayerId.MkPlayerId 1,
            AttackerDeclared.target = AttackTarget.OfBattle (ObjectId.MkObjectId 3),
            AttackerDeclared.count = 2,
            AttackerDeclared.attackingPlayer = PlayerId.MkPlayerId 0,
            AttackerDeclared.snapshot =
              ObjectSnapshot.MkObjectSnapshot
                { ObjectSnapshot.object = ObjectId.MkObjectId 1,
                  ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
                  ObjectSnapshot.controller = Just (PlayerId.MkPlayerId 0),
                  ObjectSnapshot.owner = PlayerId.MkPlayerId 1,
                  ObjectSnapshot.zone = Zone.Battlefield,
                  ObjectSnapshot.home = Zone.Battlefield,
                  ObjectSnapshot.lapsedGrants = Set.empty
                }
          }
      )
      ( " {\"attacker\":1,\"defender\":1,\"target\":{\"type\":\"OfBattle\",\"value\":3},\"count\":2,\"attackingPlayer\":0,\"snapshot\":{\"object\":1,\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"controller\":0,\"owner\":1,\"zone\":\"Battlefield\"}} "
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s AttackerDeclared.codec
