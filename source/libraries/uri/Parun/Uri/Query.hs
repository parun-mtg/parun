-- | A URI's query (RFC 3986, section 3.4) as a form's fields: each
-- key=value between ampersands, as a browser's URLSearchParams reads them.
module Parun.Uri.Query where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.List as List
import qualified Parun.Uri.Percent as Percent

-- | The query of these fields, its question mark first, empty for none; a
-- field with an empty value is its key alone.
render :: [(ByteString.ByteString, ByteString.ByteString)] -> Builder.Builder
render fields =
  let component = Percent.encode Percent.isUnreserved . Builder.byteString
      field (key, value) =
        component key
          <> if ByteString.null value then mempty else Builder.char7 '=' <> component value
   in case fields of
        [] -> mempty
        _ -> Builder.char7 '?' <> mconcat (List.intersperse (Builder.char7 '&') (fmap field fields))

-- | The fields of a query, its question mark first if any: a plus sign is a
-- space, and a key with no equals sign has the empty value.
parse :: ByteString.ByteString -> [(ByteString.ByteString, ByteString.ByteString)]
parse query =
  let body = case ByteString.stripPrefix (ByteString.singleton 0x3f) query of
        Just rest -> rest
        Nothing -> query
      decoded = Percent.decode . ByteString.map (\w -> if w == 0x2b then 0x20 else w)
      field part =
        let (key, value) = ByteString.break (== 0x3d) part
         in (decoded key, decoded (ByteString.drop 1 value))
   in fmap field . filter (not . ByteString.null) $ ByteString.split 0x26 body
