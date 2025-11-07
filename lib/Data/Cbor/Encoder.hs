module Data.Cbor.Encoder
  ( encode
  , encodeArray
  , encodeArrayStream
  , encodeByteString
  , encodeByteStringStream
  , encodeDouble
  , encodeFloat
  , encodeHalf
  , encodeMap
  , encodeMapStream
  , encodeNegative
  , encodeUnsigned
  , encodeSimple
  , encodeTag
  , encodeText
  , encodeTextStream
  ) where

import           Data.Bits
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import           Data.Cbor
import           Data.Cbor.Util
import           Data.Coerce
import           Data.Functor
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Word
import           Foreign.C.Types         (CUShort (..))
import           GHC.Float               (castDoubleToWord64, castFloatToWord32,
                                          double2Float, float2Double)
import           Numeric.Half
import           Prelude                 hiding (encodeFloat)
import qualified Data.Map.Lazy as ML

b8 :: Num a => a
b8  = 0xFF
b16 :: Num a => a
b16 = 0xFFFF
b32 :: Num a => a
b32 = 0xFFFFFFFF

-- These functions ensure that
-- we are encoding values as big-endian
encodeWord8 :: Word8 -> Builder
encodeWord8 = BSB.word8

encodeWord16 :: Word16 -> Builder
encodeWord16 = BSB.word16BE

encodeWord32 :: Word32 -> Builder
encodeWord32 = BSB.word32BE

encodeWord64 :: Word64 -> Builder
encodeWord64 = BSB.word64BE

encode :: Cbor -> Either String Builder
encode (CUnsigned w)            = pure $ encodeUnsigned w
encode (CNegative w)            = pure $ encodeNegative w
encode (CByteString s)          = pure $ encodeByteString s
encode (CByteStringStreaming s) = pure $ encodeByteStringStream s
encode (CText s)                = pure $ encodeText s
encode (CTextStreaming s)       = pure $ encodeTextStream s
encode (CHalf h)                = pure $ encodeHalf h
encode (CFloat f)               = pure $ encodeFloat f
encode (CDouble d)              = pure $ encodeDouble d
encode (CSimple w)              = encodeSimple w
encode (CArray l)               = encodeArray l
encode (CArrayStreaming l)      = encodeArrayStream l
encode (CMap m)                 = encodeMap m
encode (CMapStreaming m)        = encodeMapStream m
encode (CTag t c)               = encodeTag t c

encodeType :: Word8 -> Word8 -> Word8
encodeType m s = shiftL m 5 .|. minorBits .&. s

direct :: Integral a => Word8 -> a -> Builder
direct majorType word = BSB.word8 (encodeType majorType $ toInt word)

following8 :: Integral a => Word8 -> a -> Builder
following8 majorType word = BSB.word8 (encodeType majorType 24) <> encodeWord8 (toInt word)

following16 :: Integral a => Word8 -> a -> Builder
following16 majorType word = BSB.word8 (encodeType majorType 25) <> encodeWord16 (toInt word)

following32 :: Integral a => Word8 -> a -> Builder
following32 majorType word = BSB.word8 (encodeType majorType 26) <> encodeWord32 (toInt word)

following64 :: Integral a => Word8 -> a -> Builder
following64 majorType word = BSB.word8 (encodeType majorType 27) <> encodeWord64 (toInt word)

encodeSize :: Integral a => Word8 -> a -> Builder
encodeSize majorType size | size <= 23   = direct      majorType size
                          | size <= b8   = following8  majorType size
                          | size <= b16  = following16 majorType size
                          | size <= b32  = following32 majorType size
                          | otherwise = following64 majorType size

encodeUnsigned :: Word64 -> Builder
encodeUnsigned = encodeSize 0

encodeNegative :: Word64 -> Builder
encodeNegative = encodeSize 1

encodeByteString :: ByteString -> Builder
encodeByteString s = encodeSize 2 (BS.length s) <> BSB.byteString s

encodeText :: Text -> Builder
encodeText t = encodeSize 3 (BS.length bs) <> BSB.byteString bs
  where
    bs = encodeUtf8 t

encodeArray :: [Cbor] -> Either String Builder
encodeArray l = go <&> \s -> encodeSize 4 (length l) <> s
  where
    go = foldr f (pure mempty) l
    f a b = (<>) <$> encode a <*> b

encodeMap :: Map Cbor Cbor -> Either String Builder
encodeMap m = go <&> \s -> encodeSize 5 (M.size m) <> s
  where
    -- TODO: sort keys and filter out duplicates
    -- See section 3.7 of the RFC
    go = M.foldrWithKey f (pure mempty) m
    f k v z = (\a b c -> a <> b <> c) <$> encode k <*> encode v <*> z

encodeTag :: Word64 -> Cbor -> Either String Builder
encodeTag t c = encode c <&> \s -> encodeSize 6 t <> s

encodeHalf :: Half -> Builder
encodeHalf h = BSB.word8 (encodeType 7 25) <> encodeWord16 (coerce $ getHalf h)

encodeFloat :: Float -> Builder
encodeFloat f =
  if f == fromHalf h
  then encodeHalf h
  else BSB.word8 (encodeType 7 26) <> encodeWord32 (castFloatToWord32 f)
  where
    h = toHalf f

encodeDouble :: Double -> Builder
encodeDouble d =
  if d == float2Double f
  then encodeFloat f
  else BSB.word8 (encodeType 7 27) <> encodeWord64 (castDoubleToWord64 d)
  where
    f = double2Float d

-- This does allow encoding of Bools, null, and undefined
-- But users should probably use the stricter types for those
-- as it will not round-trip parse to the same structure.
encodeSimple :: Word8 -> Either String Builder
encodeSimple w
  | w >= 32 = pure $ BSB.word8 (encodeType 7 24) <> encodeWord8 w
  | w <= 23 = pure $ BSB.word8 (encodeType 7 w)
  | otherwise = Left "Cannot encode a simple value in the range of [24..31]"

encodeArrayStream :: [Cbor] -> Either String Builder
encodeArrayStream l = go <&> \s -> BSB.word8 (encodeType majorType 31) <> s
  where
    majorType = 4
    go = foldr f (pure (BSB.word8 breakByte)) l
    f a b = (<>) <$> encode a <*> b

encodeTextStream :: [Text] -> Builder
encodeTextStream t =
  BSB.word8 (encodeType majorType 31) <> foldr (\t' z -> encodeText t' <> z) (BSB.word8 breakByte) t
  where
    majorType = 3

encodeByteStringStream :: [ByteString] -> Builder
encodeByteStringStream s =
  BSB.word8 (encodeType majorType 31) <> foldr (\s' z -> encodeByteString s' <> z) (BSB.word8 breakByte) s
  where
    majorType = 2

encodeMapStream :: ML.Map Cbor Cbor -> Either String Builder
encodeMapStream m = go <&> \s -> BSB.word8 (encodeType 5 31) <> s
  where
    go = M.foldrWithKey f (pure $ BSB.word8 breakByte) m
    f k v z = (\a b c -> a <> b <> c) <$> encode k <*> encode v <*> z
