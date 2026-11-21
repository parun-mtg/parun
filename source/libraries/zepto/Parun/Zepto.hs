-- | A small parser-combinator library over strict 'Text.Text', in the spirit of
-- attoparsec. A parser tracks no source position: a failure keeps the input it
-- left unread, and 'parse' turns that into a line and column only once it has
-- failed, so success costs nothing for it.
module Parun.Zepto where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Text as Text

-- | Why a parse failed, and the input still unread when it did.
data Failure = MkFailure
  { remaining :: Text.Text,
    message :: String
  }
  deriving (Eq, Show)

-- | A parser's outcome: a failure, or a value and the input left after it.
data Result a
  = MkFailed Failure
  | MkDone a !Text.Text
  deriving (Eq, Show)

newtype Parser a = MkParser
  { run :: Text.Text -> Result a
  }

instance Functor Parser where
  fmap f p = MkParser $ \t -> case run p t of
    MkFailed e -> MkFailed e
    MkDone x rest -> MkDone (f x) rest
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure x = MkParser $ \t -> MkDone x t
  {-# INLINE pure #-}
  pf <*> px = MkParser $ \t -> case run pf t of
    MkFailed e -> MkFailed e
    MkDone f rest -> run (fmap f px) rest
  {-# INLINE (<*>) #-}

instance Monad Parser where
  p >>= f = MkParser $ \t -> case run p t of
    MkFailed e -> MkFailed e
    MkDone x rest -> run (f x) rest
  {-# INLINE (>>=) #-}

instance MonadFail Parser where
  fail m = MkParser $ \t -> MkFailed $ MkFailure t m
  {-# INLINE fail #-}

-- | '<|>' always backtracks: the right parser starts where the left one did.
-- When both fail, the failure that read further is kept, so the position
-- reported is the deepest one reached.
instance Applicative.Alternative Parser where
  empty = fail "no alternative"
  p <|> q = MkParser $ \t -> case run p t of
    MkFailed e1 -> case run q t of
      MkFailed e2 -> MkFailed $ furthest e1 e2
      done -> done
    done -> done
  {-# INLINE (<|>) #-}
  many p = MkParser $ manyFrom p []
  some p = (:) <$> p <*> Applicative.many p

instance Monad.MonadPlus Parser

-- | The later of two failures, by how little input each left; the first on a
-- tie, so a chain of alternatives ending in 'Applicative.empty' keeps a
-- message worth reading.
furthest :: Failure -> Failure -> Failure
furthest e1 e2 =
  if Text.length (remaining e2) < Text.length (remaining e1) then e2 else e1

-- | Repeats the parser until it fails, newest first in the accumulator. A
-- parser that succeeds without consuming loops forever.
manyFrom :: Parser a -> [a] -> Text.Text -> Result [a]
manyFrom p acc t = case run p t of
  MkFailed _ -> MkDone (reverse acc) t
  MkDone x rest -> manyFrom p (x : acc) rest

-- | The next character, when it satisfies the predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = MkParser $ \t -> case Text.uncons t of
  Just (c, rest) | f c -> MkDone c rest
  _ -> MkFailed . MkFailure t $ unexpected t
{-# INLINE satisfy #-}

-- | Exactly the given character.
char :: Char -> Parser Char
char c = MkParser $ \t -> case Text.uncons t of
  Just (d, rest) | d == c -> MkDone d rest
  _ -> MkFailed . MkFailure t $ unexpected t <> ", expecting " <> show c
{-# INLINE char #-}

-- | Any one character.
anyChar :: Parser Char
anyChar = satisfy $ const True

-- | Exactly the given text. Consumes nothing when it fails.
string :: Text.Text -> Parser Text.Text
string s = MkParser $ \t -> case Text.stripPrefix s t of
  Just rest -> MkDone s rest
  Nothing -> MkFailed . MkFailure t $ unexpected t <> ", expecting " <> show s
{-# INLINE string #-}

-- | The longest prefix whose characters all satisfy the predicate, scanned in
-- one step. Never fails; an empty match consumes nothing.
takeWhile :: (Char -> Bool) -> Parser Text.Text
takeWhile f = MkParser $ \t -> case Text.span f t of
  (match, rest) -> MkDone match rest
{-# INLINE takeWhile #-}

-- | The next character, if any, without consuming it. Branching on it commits
-- to a branch, as '<|>' does not.
peekChar :: Parser (Maybe Char)
peekChar = MkParser $ \t -> MkDone (fmap fst $ Text.uncons t) t
{-# INLINE peekChar #-}

-- | Succeeds only at the end of the input.
endOfInput :: Parser ()
endOfInput = MkParser $ \t ->
  if Text.null t
    then MkDone () t
    else MkFailed . MkFailure t $ unexpected t <> ", expecting end of input"

-- | Says what the unread input starts with.
unexpected :: Text.Text -> String
unexpected t = case Text.uncons t of
  Nothing -> "unexpected end of input"
  Just (c, _) -> "unexpected " <> show c

-- | Runs the parser over the whole input or a prefix of it. A failure's
-- message leads with its line and column, as parsec's does.
parse :: Parser a -> Text.Text -> Either String a
parse p t = case run p t of
  MkFailed e -> Left $ render t e
  MkDone x _ -> Right x

-- | Like 'parse', dropping the failure.
parseMaybe :: Parser a -> Text.Text -> Maybe a
parseMaybe p t = case run p t of
  MkFailed _ -> Nothing
  MkDone x _ -> Just x

-- | A failure in the given input, as "(line L, column C):" and its message.
render :: Text.Text -> Failure -> String
render t e =
  let (line, column) = position t e
   in "(line " <> show line <> ", column " <> show column <> "):\n" <> message e

-- | The one-based line and column where a failure in the given input occurred,
-- counted as parsec counts them: a tab advances the column to the next
-- multiple of eight, plus one.
position :: Text.Text -> Failure -> (Integer, Integer)
position t e =
  let consumed = Text.dropEnd (Text.length $ remaining e) t
      step (MkPosition line column) c = case c of
        '\n' -> MkPosition (line + 1) 1
        '\t' -> MkPosition line (column + 8 - mod (column - 1) 8)
        _ -> MkPosition line (column + 1)
   in case Text.foldl' step (MkPosition 1 1) consumed of
        MkPosition line column -> (line, column)

-- | A line and column, strict so that counting them builds no thunks.
data Position = MkPosition !Integer !Integer
  deriving (Eq, Show)
