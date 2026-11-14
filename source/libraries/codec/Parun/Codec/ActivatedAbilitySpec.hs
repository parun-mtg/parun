module Parun.Codec.ActivatedAbilitySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Parun.Codec.ActivatedAbility as ActivatedAbility
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.ActivatedAbility as ActivatedAbility
import qualified Parun.Types.ActivationRestriction as ActivationRestriction
import qualified Parun.Types.Activator as Activator
import qualified Parun.Types.Clause as Clause
import qualified Parun.Types.CombatStep as CombatStep
import qualified Parun.Types.Copyability as Copyability
import qualified Parun.Types.Cost as Cost
import qualified Parun.Types.CostAmount as CostAmount
import qualified Parun.Types.CostChoice as CostChoice
import qualified Parun.Types.CostComponent as CostComponent
import qualified Parun.Types.DefinedX as DefinedX
import qualified Parun.Types.DuringPhase as DuringPhase
import qualified Parun.Types.Effect as Effect
import qualified Parun.Types.Equip as Equip
import qualified Parun.Types.EquipTarget as EquipTarget
import qualified Parun.Types.Filter as Filter
import qualified Parun.Types.Holder as Holder
import qualified Parun.Types.Keyword as Keyword
import qualified Parun.Types.ManaCost as ManaCost
import qualified Parun.Types.ManaSymbol as ManaSymbol
import qualified Parun.Types.Modal as Modal
import qualified Parun.Types.Mode as Mode
import qualified Parun.Types.ModeMemory as ModeMemory
import qualified Parun.Types.ModeSelection as ModeSelection
import qualified Parun.Types.Optionality as Optionality
import qualified Parun.Types.Phase as Phase
import qualified Parun.Types.PhaseSelector as PhaseSelector
import qualified Parun.Types.PlayerRef as PlayerRef
import qualified Parun.Types.PlayerRelation as PlayerRelation
import qualified Parun.Types.PlayerSlot as PlayerSlot
import qualified Parun.Types.Pool as Pool
import qualified Parun.Types.Quantity as Quantity
import qualified Parun.Types.SlotName as SlotName
import qualified Parun.Types.TargetSlot as TargetSlot
import qualified Parun.Types.TurnScope as TurnScope

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'ActivatedAbility.codec' reach it only through
-- the supplied Modal codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

-- | And the `ability` parameter likewise: it too is reached only through the
-- codec supplied here, so any type proves the shape.
abilityCodec :: Codec.Codec Text.Text
abilityCodec = Common.text

codec :: Codec.Codec (ActivatedAbility.ActivatedAbility Text.Text Text.Text)
codec = ActivatedAbility.codec cardCodec abilityCodec

toJson :: ActivatedAbility.ActivatedAbility Text.Text Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (ActivatedAbility.ActivatedAbility Text.Text Text.Text)
fromJson = Codec.decode codec

-- One constructor, so a case per key: rule 702.6a's equip ability as
-- Parun.Engine.Keyword mints it, carrying CR 602.5d's SorcerySpeed clause, CR
-- 117.1b's default of no clause at all,
-- whose key is elided, Kongming's Contraptions' TWO clauses, which is what
-- the key being an array is for, the same equip ability carrying the stamp
-- its minter put on it, a cost choice (CR 602.2b), a defined X and an announced player.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ActivatedAbility" $ do
  Spec.it s "MkActivatedAbility, rule 702.6a's equip ability" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
          []
          []
          Nothing
          ( Modal.MkModal
              ( Seq.singleton
                  ( Mode.MkMode
                      (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))))
                      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSlot.required Pool.Creatures (Just (Filter.HeldBy Holder.Controller PlayerRelation.You))))
                  )
              )
              (ModeSelection.ChooseExactly 1)
              ModeMemory.NoMemory
              Seq.empty
          )
          [ActivationRestriction.SorcerySpeed]
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"modal\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"Attach\",\"value\":\"target\"}]}],\"targetSlots\":{\"target\":{\"pool\":{\"type\":\"Creatures\"},\"filter\":{\"type\":\"HeldBy\",\"value\":{\"holder\":\"Controller\",\"relation\":\"You\"}}}}}]},\"restrictions\":[{\"type\":\"SorcerySpeed\"}]} "
  -- CR 602.2b: a choice printed inside the activation cost, Bullseye, Death
  -- Dealer's shape, rides `costChoices` beside the shared cost.
  Spec.it s "MkActivatedAbility with a cost choice" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) [CostComponent.TapThis])
          [CostChoice.MkCostChoice (Cost.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.PayLife (CostAmount.Fixed 2)] NonEmpty.:| [Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []])]
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          []
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}],\"components\":[{\"type\":\"TapThis\"}]},\"costChoices\":[[{\"mana\":[],\"components\":[{\"type\":\"PayLife\",\"value\":{\"type\":\"Fixed\",\"value\":2}}]},{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}]],\"modal\":{\"modes\":[{}]}} "
  -- CR 113.6g: Gogo, Master of Mimicry's "This ability can't be copied".
  Spec.it s "MkActivatedAbility that can't be copied" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.TapThis])
          []
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          []
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.CantBeCopied
          Nothing
      )
      " {\"cost\":{\"mana\":[],\"components\":[{\"type\":\"TapThis\"}]},\"modal\":{\"modes\":[{}]},\"copyability\":\"CantBeCopied\"} "
  -- CR 602.5's conjunction, in the JSON: two clauses in printed order, which is
  -- the shape a single tagged object could not hold.
  Spec.it s "MkActivatedAbility, Kongming's Contraptions' two clauses" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [CostComponent.TapThis])
          []
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          [ ActivationRestriction.DuringPhase (DuringPhase.MkDuringPhase (PhaseSelector.Step (Phase.Combat CombatStep.DeclareAttackers)) (TurnScope.PlayersTurn (PlayerRef.Relative PlayerRelation.AnyPlayer))),
            ActivationRestriction.AttackedThisStep
          ]
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":null,\"components\":[{\"type\":\"TapThis\"}]},\"modal\":{\"modes\":[{}]},\"restrictions\":[{\"type\":\"DuringPhase\",\"value\":{\"window\":{\"type\":\"Step\",\"value\":{\"type\":\"Combat\",\"value\":\"DeclareAttackers\"}},\"scope\":{\"type\":\"PlayersTurn\",\"value\":{\"type\":\"Relative\",\"value\":\"AnyPlayer\"}}}},{\"type\":\"AttackedThisStep\"}]} "
  -- CR 702.6a: the stamp Parun.Engine.Keyword.mintedBy writes, which is the one
  -- key no card in data/cards/ carries -- an ability already on the stack has to
  -- round-trip it, since Parun.Codec.GameState reaches this codec through
  -- Parun.Codec.Object.
  Spec.it s "the keyword an ability was minted from round-trips" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
          []
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          [ActivationRestriction.SorcerySpeed]
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          (Just (Keyword.Equip (Equip.MkEquip (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []) Nothing EquipTarget.Creature Nothing)))
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"modal\":{\"modes\":[{}]},\"restrictions\":[{\"type\":\"SorcerySpeed\"}],\"keyword\":{\"type\":\"Equip\",\"value\":{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":1}]},\"quality\":null}}} "
  -- CR 117.1b: no rider is the default for nearly every ability, so the key
  -- stays out of the JSON.
  Spec.it s "an unrestricted ability omits the restrictions key, and an absent key decodes to none" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [])
          []
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          []
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":null},\"modal\":{\"modes\":[{}]}} "
  -- CR 107.3c: Soul Foundry's "X is the mana value of that card" is the key's
  -- producer; Nothing stays out of the JSON, as above.
  Spec.it s "an ability's defined X round-trips" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [])
          []
          []
          (Just (DefinedX.MkDefinedX (Quantity.Literal 2) Nothing))
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          []
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          Nothing
      )
      " {\"cost\":{\"mana\":null},\"definedX\":{\"quantity\":{\"type\":\"Literal\",\"value\":2}},\"modal\":{\"modes\":[{}]}} "
  -- CR 115.10a: Lodestone Bauble's "a player", chosen as it is announced.
  Spec.it s "an ability's announced player choice round-trips" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [])
          []
          []
          Nothing
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1) ModeMemory.NoMemory Seq.empty)
          []
          (Activator.Related PlayerRelation.You)
          Nothing
          Nothing
          Nothing
          Copyability.Copyable
          (Just (PlayerSlot.MkPlayerSlot PlayerRelation.AnyPlayer (SlotName.MkSlotName (Text.pack "player"))))
      )
      " {\"cost\":{\"mana\":null},\"modal\":{\"modes\":[{}]},\"chosenPlayer\":{\"players\":\"AnyPlayer\",\"slot\":\"player\"}} "
