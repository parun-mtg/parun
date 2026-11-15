module Parun.Codec.ViewIsSpec where

import qualified Data.Text as Text
import qualified Parun.Codec.ViewIs as ViewIs
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.Spec as Spec
import qualified Parun.Types.Label as Label.Type
import qualified Parun.Types.Reply as Reply.Type
import qualified Parun.Types.View as View.Type
import qualified Parun.Types.ViewIs as ViewIs.Type
import qualified Parun.Types.Zone as Zone.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Parun.Codec.ViewIs" $ do
  Spec.it s "a view with no subject" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Stack Nothing Nothing Nothing (Reply.Type.Array [])) " {\"of\":\"Stack\",\"is\":[]} "
  Spec.it s "a player's zone" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Zone (Just (Label.Type.MkLabel (Text.pack "bob"))) Nothing (Just Zone.Type.Hand) (Reply.Type.Array [Reply.Type.Text (Text.pack "Hill Giant")])) " {\"of\":\"Zone\",\"player\":\"bob\",\"zone\":\"Hand\",\"is\":[\"Hill Giant\"]} "
  Spec.it s "a prompt, by kind and offer" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Prompt Nothing Nothing Nothing (Reply.Type.Object [(Text.pack "kind", Reply.Type.Text (Text.pack "RollDie")), (Text.pack "offer", Reply.Type.Object [(Text.pack "sides", Reply.Type.Number 20)])])) " {\"of\":\"Prompt\",\"is\":{\"kind\":\"RollDie\",\"offer\":{\"sides\":20}}} "
  Spec.it s "the prompts asked, by decider and kind" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Asked Nothing Nothing Nothing (Reply.Type.Array [Reply.Type.Array [Reply.Type.Text (Text.pack "alice"), Reply.Type.Text (Text.pack "ChooseDiscard")], Reply.Type.Array [Reply.Type.Null, Reply.Type.Text (Text.pack "Shuffle")]])) " {\"of\":\"Asked\",\"is\":[[\"alice\",\"ChooseDiscard\"],[null,\"Shuffle\"]]} "
  Spec.it s "a player's designations" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Designations (Just (Label.Type.MkLabel (Text.pack "alice"))) Nothing Nothing (Reply.Type.Array [Reply.Type.Text (Text.pack "CitysBlessing")])) " {\"of\":\"Designations\",\"player\":\"alice\",\"is\":[\"CitysBlessing\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ViewIs.codec
