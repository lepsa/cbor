module Data.Cbor.Parser (cbor) where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Cbor
import           Data.Either
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import           Data.Parser
import           Data.Text           (Text)
import           Data.Text.Encoding  (decodeUtf8')
import           Data.Word
import           Foreign.C.Types     (CUShort (..))
import           GHC.Float
import           Numeric.Half
import           Prelude             (Integral, Num, fromInteger, otherwise,
                                      show, toInteger, uncurry, (+))

reserved :: Parser s a
reserved = Parser $ \_ -> Left $ errorUnexpected "reserved"

malformed :: Parser s a
malformed = Parser $ \_ -> Left $ errorUnexpected "malformed"

bitError :: Parser s a
bitError = Parser $ \_ -> Left $ errorUnexpected "bit error"

--
-- CBOR specific parsing
--

minorBits :: Word8
minorBits = 0b00011111

toInt :: (Integral a, Num b) => a -> b
toInt = fromInteger . toInteger

decodeType :: Word8 -> (Word8, Word8)
decodeType x = (x `shiftR` 5, x .&. minorBits)

cbreak :: Uncons s => Parser s ()
cbreak = void $ byte 0b111_11111

cbor :: ParserInput s => Parser s Cbor
cbor = do
  (mt, sc) <- decodeType <$> anyByte
  case mt of
    0 -> CUnsigned <$> unsigned sc
    1 -> CNegative <$> negative sc
    2 -> CByteString <$> bytestring sc
    3 -> CText <$> text sc
    4 -> CArray <$> array sc
    5 -> CMap <$> map sc
    6 -> uncurry CTag <$> tag sc
    7 -> floating sc
    _ -> bitError

read8 :: Uncons s => Parser s Word8
read8 = anyByte

read16 :: Uncons s => Parser s Word16
read16 = do
  b1 <- toInt <$> read8
  b2 <- toInt <$> read8
  pure $ shift b1 8 + b2

read32 :: Uncons s => Parser s Word32
read32 = do
  b1 <- toInt <$> read16
  b2 <- toInt <$> read16
  pure $ shift b1 16 + b2

read64 :: Uncons s => Parser s Word64
read64 = do
  b1 <- toInt <$> read32
  b2 <- toInt <$> read32
  pure $ shift b1 32 + b2

itemLength :: Uncons s => Word8 -> Parser s (Maybe Word64)
itemLength w | w <= 23 = pure . pure $ toInt w
             | w == 24 = pure . toInt <$> read8
             | w == 25 = pure . toInt <$> read16
             | w == 26 = pure . toInt <$> read32
             | w == 27 = pure <$> read64
             | w <= 31 = pure Nothing
             | otherwise = bitError

unsigned :: Uncons s => Word8 -> Parser s Word64
unsigned =
  itemLength >=>
  maybe
    malformed
    pure

-- The semantic value of this is -1 - value.
-- To give the range [-2^64..-1]
-- Be careful when using this
negative :: Uncons s => Word8 -> Parser s Word64
negative = unsigned

bytestringChunk :: Uncons s => Parser s ByteString
bytestringChunk = do
  (mt, sc) <- decodeType <$> anyByte
  when (mt /= 2) malformed
  l <- itemLength sc
  case l of
    Nothing  -> malformed
    (Just a) -> BS.pack <$> count (toInt a) anyByte

bytestring :: ParserInput s => Word8 -> Parser s ByteString
bytestring =
  itemLength >=>
  maybe go f
  where
    go = do
      l <- manyTill bytestringChunk (try cbreak)
      pure $ fold l
    f l = BS.pack <$> count (toInt l) anyByte

textChunk :: ParserInput s => Parser s Text
textChunk = do
  (mt, sc) <- decodeType <$> anyByte
  when ( mt /= 3) malformed
  l <- itemLength sc
  case l of
    Nothing -> malformed
    (Just a) ->
      count (toInt a) anyByte >>=
      either
        (unexpected . show)
        pure
        . decodeUtf8'
        . BS.pack

text :: ParserInput s => Word8 -> Parser s Text
text =
  itemLength >=>
  maybe go f
  where
    go = do
      l <- manyTill textChunk (try cbreak)
      pure $ fold l
    f l =
      count (toInt l) anyByte >>=
      either
        (unexpected . show)
        pure
        . decodeUtf8'
        . BS.pack

array :: ParserInput s => Word8 -> Parser s [Cbor]
array =
  itemLength >=>
  maybe go (\n -> count (toInt n) cbor)
  where
    go = manyTill cbor (try cbreak)

map :: ParserInput s => Word8 -> Parser s (Map Cbor Cbor)
map = fmap M.fromList . go
  where
    go = itemLength >=> maybe
      (manyTill pair $ try cbreak)
      (\n -> count (toInt n) pair)
    pair = (,) <$> cbor <*> cbor

tag :: ParserInput s => Word8 -> Parser s (Word64, Cbor)
tag = itemLength >=> maybe malformed go
  where
    go t = (t,) <$> cbor

floating :: ParserInput s => Word8 -> Parser s Cbor
floating w | w <= 23 = pure $ CSimple w
            | w == 24 = simple
            | w == 25 = CHalf . Half . CUShort <$> read16
            | w == 26 = CFloat . castWord32ToFloat <$> read32
            | w == 27 = CDouble . castWord64ToDouble <$> read64
            | w <= 30 = reserved
            | w == 31 = unexpected "break found while parsing float/simple"
            | otherwise = bitError

simple :: ParserInput s => Parser s Cbor
simple = anyByte >>= f
  where
    f b | b <= 31 = unexpected "Found simple type with an invalid value. Major type 7, additional information 24, and a value of 31 or less."
        | otherwise = pure $ CSimple b

