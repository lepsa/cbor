module Data.Cbor.Parser where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Cbor
import Data.Cbor.Util
import Data.Map (Map)
import Data.Map qualified as M
import Data.Parser
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Word
import Foreign.C.Types (CUShort (..))
import GHC.Float
import Numeric.Half

reserved :: Parser a
reserved = Parser $ \_ -> Left $ Unexpected "reserved"

malformed :: Parser a
malformed = Parser $ \_ -> Left $ Unexpected "malformed"

bitError :: Parser a
bitError = Parser $ \_ -> Left $ Unexpected "bit error"

--
-- CBOR specific parsing
--

decodeType :: Word8 -> (Word8, Word8)
decodeType x = (x `shiftR` 5, x .&. minorBits)

packType :: (Word8, Word8) -> Word8
packType (mt, eb) = mt' .|. eb'
  where
    mt' = (mt `shiftL` 5) .&. majorBits
    eb' = eb .&. minorBits

cbreak :: Parser ()
cbreak = void $ byte 0b111_11111

cbor :: Parser Cbor
cbor = do
  (mt, sc) <- decodeType <$> anyByte
  case mt of
    0 -> CUnsigned <$> cunsigned sc
    1 -> CNegative <$> cnegative sc
    2 -> CByteString <$> cbytestring sc
    3 -> CText <$> ctext sc
    4 -> CArray <$> carray sc
    5 -> CMap <$> cmap sc
    6 -> uncurry CTag <$> ctag sc
    7 -> cfloating sc
    _ -> bitError

read8 :: Parser Word8
read8 = anyByte

read16 :: Parser Word16
read16 = do
  b1 <- toInt <$> read8
  b2 <- toInt <$> read8
  pure $ shift b1 8 + b2

read32 :: Parser Word32
read32 = do
  b1 <- toInt <$> read16
  b2 <- toInt <$> read16
  pure $ shift b1 16 + b2

read64 :: Parser Word64
read64 = do
  b1 <- toInt <$> read32
  b2 <- toInt <$> read32
  pure $ shift b1 32 + b2

itemLength :: Word8 -> Parser (Maybe Word64)
itemLength w
  | w <= 23 = pure . pure $ toInt w
  | w == 24 = pure . toInt <$> read8
  | w == 25 = pure . toInt <$> read16
  | w == 26 = pure . toInt <$> read32
  | w == 27 = pure <$> read64
  | w <= 31 = pure Nothing
  | otherwise = bitError

cunsigned :: Word8 -> Parser Word64
cunsigned =
  itemLength
    >=> maybe
      malformed
      pure

-- The semantic value of this is -1 - value.
-- To give the range [-2^64..-1]
-- Be careful when using this
cnegative :: Word8 -> Parser Word64
cnegative = fmap (+1) . cunsigned

cbytestringChunk :: Parser ByteString
cbytestringChunk = do
  (mt, sc) <- decodeType <$> anyByte
  when (mt /= 2) malformed
  l <- itemLength sc
  case l of
    Nothing -> malformed
    (Just a) -> BS.pack <$> count (toInt a) anyByte

cbytestring :: Word8 -> Parser ByteString
cbytestring =
  itemLength
    >=> maybe go f
  where
    go = do
      l <- manyTill cbytestringChunk (try cbreak)
      pure $ foldr (<>) mempty l
    f l = BS.pack <$> count (toInt l) anyByte

ctextChunk :: Parser Text
ctextChunk = do
  (mt, sc) <- decodeType <$> anyByte
  when (mt /= 3) malformed
  l <- itemLength sc
  case l of
    Nothing -> malformed
    Just a ->
      count (toInt a) anyByte
        >>= either
          (unexpected . show)
          pure
          . decodeUtf8'
          . BS.pack

ctext :: Word8 -> Parser Text
ctext =
  itemLength
    >=> maybe go f
  where
    go = do
      l <- manyTill ctextChunk (try cbreak)
      pure $ foldr (<>) mempty l
    f l =
      count (toInt l) anyByte
        >>= either
          (unexpected . show)
          pure
          . decodeUtf8'
          . BS.pack

carray :: Word8 -> Parser [Cbor]
carray =
  itemLength
    >=> maybe go (\n -> count (toInt n) cbor)
  where
    go = manyTill cbor (try cbreak)

cmap :: Word8 -> Parser (Map Cbor Cbor)
cmap = fmap M.fromList . go
  where
    go =
      itemLength
        >=> maybe
          (manyTill pair $ try cbreak)
          (\n -> count (toInt n) pair)
    pair = (,) <$> cbor <*> cbor

ctag :: Word8 -> Parser (Word64, Cbor)
ctag = itemLength >=> maybe malformed go
  where
    go t = (t,) <$> cbor

cfloating :: Word8 -> Parser Cbor
cfloating w
  | w <= 19 = pure $ CSimple w
  | w == 20 = pure CFalse
  | w == 21 = pure CTrue
  | w == 22 = pure CNull
  | w == 23 = pure CUndefined
  | w == 24 = csimple
  | w == 25 = CHalf . Half . CUShort <$> read16
  | w == 26 = CFloat . castWord32ToFloat <$> read32
  | w == 27 = CDouble . castWord64ToDouble <$> read64
  | w <= 30 = reserved
  | w == 31 = unexpected "break found while parsing float/simple"
  | otherwise = bitError

csimple :: Parser Cbor
csimple = anyByte >>= f
  where
    f b
      | b <= 31 = unexpected "Found simple type with an invalid value. Major type 7, additional information 24, and a value of 31 or less."
      | otherwise = pure $ CSimple b
