module Parun.Codec.PlayersWhoSpec where

import qualified Parun.Codec.PlayersWho as PlayersWho
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.CardType as CardType
import qualified Parun.Types.Color as Color
import qualified Parun.Types.Compares as Compares
import qualified Parun.Types.Comparison as Comparison
import qualified Parun.Types.Condition as Condition
import qualified Parun.Types.ControlMargin as ControlMargin
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayersWho as PlayersWho
import qualified Parun.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.PlayersWho" $ do
  Spec.it s "MkPlayersWho, unnarrowed, and the key is omitted" $
    Common.assertCodec
      s
      PlayersWho.codec
      PlayersWho.MkPlayersWho {PlayersWho.players = PlayerRef.Relative PlayerRelation.You, PlayersWho.who = Nothing, PlayersWho.whose = Nothing}
      " {\"players\":{\"type\":\"Relative\",\"value\":\"You\"}} "
  -- Disorder's "each player who controls a white creature".
  Spec.it s "MkPlayersWho, narrowed by a player filter" $
    Common.assertCodec
      s
      PlayersWho.codec
      PlayersWho.MkPlayersWho
        { PlayersWho.players = PlayerRef.Relative PlayerRelation.AnyPlayer,
          PlayersWho.who = Just (Filter.Controls (ControlMargin.AtLeast 1) (Filter.And [Filter.HasCardType CardType.Creature, Filter.HasColor Color.White])),
          PlayersWho.whose = Nothing
        }
      " {\"players\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"},\"who\":{\"type\":\"Controls\",\"value\":{\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":\"Creature\"},{\"type\":\"HasColor\",\"value\":\"White\"}]},\"margin\":{\"type\":\"AtLeast\",\"value\":1}}}} "
  -- Outpace Oblivion's "each player who doesn't have max speed".
  Spec.it s "MkPlayersWho, narrowed by a condition" $
    Common.assertCodec
      s
      PlayersWho.codec
      PlayersWho.MkPlayersWho
        { PlayersWho.players = PlayerRef.Relative PlayerRelation.AnyPlayer,
          PlayersWho.who = Nothing,
          PlayersWho.whose = Just (Condition.Compares (Compares.MkCompares (Quantity.Speed PlayerRef.Candidate) Comparison.AtMost (Quantity.Literal 3)))
        }
      " {\"players\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"},\"whose\":{\"type\":\"Compares\",\"value\":{\"comparison\":\"AtMost\",\"measured\":{\"type\":\"Speed\",\"value\":{\"type\":\"Candidate\"}},\"threshold\":{\"type\":\"Literal\",\"value\":3}}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s PlayersWho.codec
