module Data.Parser where

import           Control.Applicative
import           Data.Bool
import qualified Data.ByteString     as BS
import           Data.Functor
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Word
import qualified Data.ByteString.Lazy as LBS

class Alternative m => Parsing m where
  try           :: m a -> m a
  skipMany      :: m a -> m ()
  skipSome      :: m a -> m ()
  unexpected    :: String -> m a
  eof           :: m ()
  notFollowedBy :: Show a => m a -> m ()


class Uncons s where
  uncons :: s -> Maybe (Word8, s)

instance Uncons BS.ByteString where
  uncons = BS.uncons

instance Uncons LBS.ByteString where
  uncons = LBS.uncons

newtype Error = Error
  ( forall r
    .  r -- EOF
    -> r -- Empty
    -> (Word8 -> r) -- UnexpectedByte
    -> (String -> r) -- Unexpected
    -> r
  )

errorEof :: Error
errorEof = Error $ \e _ _ _ -> e

errorEmpty :: Error
errorEmpty = Error $ \_ e _ _ -> e

errorUnexpectedByte :: Word8 -> Error
errorUnexpectedByte w = Error $ \_ _ e _ -> e w

errorUnexpected :: String -> Error
errorUnexpected s = Error $ \_ _ _ e -> e s

instance Eq Error where
  (Error a) == (Error b) = a
    (b True False (const False) (const False))
    (b False True (const False) (const False))
    (const $ b False False (const True) (const False))
    (const $ b False False (const False) (const True))

instance Show Error where
  show (Error e) = e
    "EOF"
    "Empty"
    (\w -> "UnexpectedByte " <> show w)
    (\s -> "Unexpected " <> show s)

newtype Parser s a = Parser {
  runParser :: s -> Either Error (s, a)
}

type ParserInput p = (Uncons p, Monoid p, Show p)

instance Functor (Parser s) where
  fmap f p = Parser $ fmap (fmap f) . runParser p

instance Applicative (Parser s) where
  pure a = Parser $ \s -> pure (s, a)
  pf <*> pa = Parser $ \s -> do
    (s', f)  <- runParser pf s
    (s'', a) <- runParser pa s'
    pure (s'', f a)

instance Alternative (Parser s) where
  empty = Parser $ \_ -> Left errorEmpty
  p <|> q = Parser $ \s ->
    let
      f e'@(Error e) = e
        (Left e')
        (runParser q s)
        (\_ -> Left e')
        (\_ -> Left e')
    in
      either f pure $ runParser p s

instance Monad (Parser s) where
  p >>= f = Parser $ \s -> do
    (s', a) <- runParser p s
    runParser (f a) s'

instance ParserInput s => Parsing (Parser s) where
  try p = Parser $
    either
      (const $ Left errorEmpty)
      pure
      . runParser p
  skipMany p = p *> skipMany p <|> pure ()
  skipSome p = p *> skipMany p
  unexpected = Parser . const . Left . errorUnexpected
  eof = Parser $ \s -> case uncons s of
    Nothing     -> pure (mempty, ())
    Just (c, _) -> Left $ errorUnexpectedByte c
  notFollowedBy m = Parser $ \s -> either
    (\_ -> pure (s, ()))
    (Left . errorUnexpected . show)
    $ runParser m s

choice :: Alternative m => [m a] -> m a
choice = asum

option :: Alternative m => a -> m a -> m a
option a m = m <|> pure a

skipOptional :: Alternative m => m a -> m ()
skipOptional m = void m <|> pure ()

between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket m = bra *> m <* ket

surroundedBy :: Applicative m => m a -> m sur -> m a
surroundedBy m sur = sur *> m <* sur

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy m s = go <|> pure []
  where
    go = (:) <$> m <*> many (s *> m)

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 m s = (:) <$> m <*> many (s *> m)

sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty m s = (:|) <$> m <*> many (s *> m)

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 m s = (:) <$> m <*> many (s *> m) <* skipOptional s

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty m s = (:|) <$> m <*> many (s *> m) <* skipOptional s

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy m s = sepEndBy1 m s <|> pure []

endBy1 :: Alternative m => m a -> m sep -> m [a]
endBy1 m s = (:) <$> m' <*> many m'
  where
    m' = m <* s

endByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
endByNonEmpty m s = (:|) <$> m' <*> many m'
  where
    m' = m <* s

endBy :: Alternative m => m a -> m sep -> m [a]
endBy m s = endBy1 m s <|> pure []

count :: Applicative m => Int -> m a -> m [a]
count n m
  | n <= 0 = pure []
  | otherwise = (:) <$> m <*> count (n - 1) m

manyTill :: Alternative m => m a -> m end -> m [a]
manyTill m e = go where go = [] <$ e <|> (:) <$> m <*> go

byte :: Uncons s => Word8 -> Parser s Word8
byte = satisfy . (==)

anyByte :: Uncons s => Parser s Word8
anyByte = satisfy (const True)

satisfy :: Uncons s => (Word8 -> Bool) -> Parser s Word8
satisfy f = Parser $ \s -> case uncons s of
  Nothing -> Left errorEof
  Just (b, s') -> bool
    (Left $ errorUnexpectedByte b)
    (pure (s', b))
    $ f b
