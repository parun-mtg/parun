-- Covers data/cards/*.json and the file-naming rule, Parun.Registry.filedAs and
-- Parun.Registry.cardPath -- through which it reaches Parun.Slug.fromText, whose
-- own spec is Parun.SlugSpec.
module Parun.CardsSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Parun.Codec.Card as Card
import qualified Parun.Codec.Printing as Printing
import qualified Parun.Json.Array as Array
import qualified Parun.Json.Object as Object
import qualified Parun.Json.Pair as Pair
import qualified Parun.Json.String as String
import qualified Parun.Json.Value as Value
import qualified Parun.JsonCodec.Codec as Codec
import qualified Parun.JsonCodec.Common as Common
import qualified Parun.JsonSchema.Define as Define
import qualified Parun.JsonSchema.Validate as Validate
import qualified Parun.Registry as Registry
import qualified Parun.Spec as Spec
import qualified Parun.Types.Printing as Printing

spec :: (MonadIO.MonadIO n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Parun.Cards" $ do
  -- Listed, not parsed: each case reads its own file, so building the test
  -- tree decodes nothing and a narrow -p pays for no corpus parse.
  root <- MonadIO.liftIO Registry.defaultRoot
  paths <- MonadIO.liftIO (Registry.listRoot root)

  -- Built once for the whole corpus rather than once per file: running the
  -- schema builds every $defs entry, which does not depend on the card.
  let schema = Define.run (Codec.schema Card.codec)

  Monad.forM_ paths $ \path ->
    Spec.it s (reverse (drop (length ".json") (takeWhile (/= '/') (reverse path)))) $ do
      checkFile s root schema path

checkFile :: Spec.Spec IO n -> FilePath -> Define.Document -> FilePath -> IO ()
checkFile s root schema path = do
  -- Read as bytes and decoded as UTF-8 explicitly, for the reason
  -- Parun.Registry.parseCard is: Data.Text.IO.readFile decodes using the locale
  -- encoding, which is ASCII under LC_ALL=C, so this would otherwise die on
  -- khabal-ghoul.json's "á". Not parseCard alone -- this case wants the decoded
  -- JSON value to compare against, not only a Card.
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> Spec.assertFailure s (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Common.parse contents of
        Left err -> Spec.assertFailure s (path <> ": " <> Text.unpack err)
        Right value -> do
          -- Every committed file matches the schema the card codec publishes.
          -- Asserted BEFORE the decode and re-encoding below, which would
          -- otherwise absorb a schema defect and report itself instead.
          -- Card.codec rather than Printing.codec because Registry.parseCard is
          -- what reads these files and it decodes a Card; the two write the
          -- same wire.
          Spec.assertEqWith s (path <> ": schema") (Validate.validate schema value) []
          case Registry.parseCard bytes of
            Left err -> Spec.assertFailure s (path <> ": " <> Text.unpack err)
            Right card -> do
              let p = Printing.ofCard card
              -- The file lives where Registry.filedAs says a card with these
              -- faces is filed.
              Spec.assertEqWith s (path <> ": filed as") path (Registry.cardPath root (Registry.filedAs card))
              -- The loader reads everything the file says and invents nothing:
              -- re-encoding the loaded printing reproduces the file's meaning. Compared
              -- up to key order and whitespace, because JSON objects are unordered and
              -- formatting is not part of the contract. The corpus is committed
              -- pretty-printed (`jq -S .`) while Common.render emits compact output, so
              -- this can never quietly regress into a byte comparison: every file would
              -- fail at once.
              -- The paths that differ come first, so a failure names the field
              -- rather than printing both cards whole.
              let encoded = Codec.encode Printing.codec p
              Spec.assertEqWith s (path <> ": re-encoded, then on file") (differences "$" encoded value) []
              Spec.assertEqWith s path (Common.sortKeys encoded) (Common.sortKeys value)
              -- And the other way: the printing's encoding decodes back to it,
              -- straight and through the text it renders to.
              Spec.assertEqWith s (path <> ": round trip") (Codec.decode Printing.codec encoded) (Right p)
              Spec.assertEqWith s (path <> ": round trip through text") (Common.parse (Common.render encoded) >>= Codec.decode Printing.codec) (Right p)

-- | Where two JSON values part, as (path, left, right) with each side rendered;
-- Nothing is a side lacking the path. Objects are compared by key, arrays by
-- index.
differences :: String -> Value.Value -> Value.Value -> [(String, Maybe Text.Text, Maybe Text.Text)]
differences at left right = case (left, right) of
  (Value.Object l, Value.Object r) ->
    let fields o = [(Text.unpack (String.unwrap (Pair.name f)), Pair.value f) | f <- Object.unwrap o]
        ls = fields l
        rs = fields r
     in concatMap (\k -> side (at <> "." <> k) (lookup k ls) (lookup k rs)) (Set.toList (Set.fromList (fmap fst (ls <> rs))))
  (Value.Array l, Value.Array r) ->
    let ls = Array.unwrap l
        rs = Array.unwrap r
        index i = Maybe.listToMaybe . drop i
     in concatMap (\i -> side (at <> "[" <> show i <> "]") (index i ls) (index i rs)) [0 .. max (length ls) (length rs) - 1]
  _ -> if left == right then [] else [(at, Just (Common.render left), Just (Common.render right))]
  where
    side path (Just a) (Just b) = differences path a b
    side path a b = [(path, fmap Common.render a, fmap Common.render b)]
