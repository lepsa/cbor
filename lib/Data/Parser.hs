module Data.Parser where

import Control.Applicative
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty
import Data.Word

class (Alternative m) => Parsing m where
  try           :: m a -> m a
  skipMany      :: m a -> m ()
  skipSome      :: m a -> m ()
  unexpected    :: String -> m a
  eof           :: m ()
  notFollowedBy :: (Show a) => m a -> m ()

data Error
  = EOF
  | Empty
  | UnexpectedByte Word8
  | Unexpected String
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: ByteString -> Either Error (ByteString, a)
  }

instance Functor Parser where
  fmap f p = Parser $ \s ->
    fmap f <$> runParser p s

instance Applicative Parser where
  pure a = Parser $ \s -> pure (s, a)
  pf <*> pa = Parser $ \s -> do
    (s', f) <- runParser pf s
    (s'', a) <- runParser pa s'
    pure (s'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Left Empty
  p <|> q = Parser $ \s ->
    let f Empty = runParser q s
        f e = Left e
     in either f pure $ runParser p s

instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (s', a) <- runParser p s
    runParser (f a) s'

instance Parsing Parser where
  try p =
    Parser $
      either
        (const $ Left Empty)
        pure
        . runParser p
  skipMany p = (p *> skipMany p) <|> pure ()
  skipSome p = p *> skipMany p
  unexpected = Parser . const . Left . Unexpected
  eof = Parser $ \s -> case BS.uncons s of
    Nothing -> pure (mempty, ())
    Just (c, _) -> Left $ UnexpectedByte c
  notFollowedBy m = Parser $ \s ->
    either
      (\_ -> pure (s, ()))
      (Left . Unexpected . show)
      $ runParser m s

choice :: (Alternative m) => [m a] -> m a
choice = foldr (<|>) empty

option :: (Alternative m) => a -> m a -> m a
option a m = m <|> pure a

skipOptional :: (Alternative m) => m a -> m ()
skipOptional m = () <$ m <|> pure ()

between :: (Applicative m) => m bra -> m ket -> m a -> m a
between bra ket m = bra *> m <* ket

surroundedBy :: (Applicative m) => m a -> m sur -> m a
surroundedBy m sur = sur *> m <* sur

sepBy :: (Alternative m) => m a -> m sep -> m [a]
sepBy m s = go <|> pure []
  where
    go = (:) <$> m <*> many (s *> m)

sepBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepBy1 m s = (:) <$> m <*> many (s *> m)

sepByNonEmpty :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty m s = (:|) <$> m <*> many (s *> m)

sepEndBy1 :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy1 m s = (:) <$> m <*> many (s *> m) <* skipOptional s

sepEndByNonEmpty :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty m s = (:|) <$> m <*> many (s *> m) <* skipOptional s

sepEndBy :: (Alternative m) => m a -> m sep -> m [a]
sepEndBy m s = sepEndBy1 m s <|> pure []

endBy1 :: (Alternative m) => m a -> m sep -> m [a]
endBy1 m s = (:) <$> m' <*> many m'
  where
    m' = m <* s

endByNonEmpty :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
endByNonEmpty m s = (:|) <$> m' <*> many m'
  where
    m' = m <* s

endBy :: (Alternative m) => m a -> m sep -> m [a]
endBy m s = endBy1 m s <|> pure []

count :: (Applicative m) => Int -> m a -> m [a]
count n m
  | n <= 0 = pure []
  | otherwise = (:) <$> m <*> count (n - 1) m

manyTill :: (Alternative m) => m a -> m end -> m [a]
manyTill m e = go where go = ([] <$ e) <|> ((:) <$> m <*> go)

byte :: Word8 -> Parser Word8
byte = satisfy . (==)

anyByte :: Parser Word8
anyByte = satisfy (const True)

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = Parser $ \s -> case BS.uncons s of
  Nothing -> Left EOF
  Just (b, s') ->
    bool
      (Left $ UnexpectedByte b)
      (pure (s', b))
      $ f b
