{-# LANGUAGE OverloadedLists #-}

module Data.Cbor.Serialiser where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Cbor
import Data.Cbor.Util
import Data.Coerce
import Data.Functor
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import Foreign.C.Types (CUShort (..))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Numeric.Half
import Prelude hiding (encodeFloat)

b8 :: (Num a) => a
b8 = 0xFF

b16 :: (Num a) => a
b16 = 0xFFFF

b32 :: (Num a) => a
b32 = 0xFFFFFFFF

b64 :: (Num a) => a
b64 = 0xFFFFFFFFFFFFFFFF

-- These functions ensure that
-- we are encoding values as big-endian
encodeWord8 :: Word8 -> ByteString
encodeWord8 w = [w]

encodeWord16 :: Word16 -> ByteString
encodeWord16 w =
  -- Mask out the most significant byte and put it first
  encodeWord8 (toInt $ ((shiftL b8 8) .&. w) `shiftR` 8)
    <>
    -- Mask out the least significate byte and put it last
    encodeWord8 (toInt $ b8 .&. w)

-- Same idea as encodeWord16, we crack the value in half and recurse
encodeWord32 :: Word32 -> ByteString
encodeWord32 w =
  encodeWord16 (toInt $ ((shiftL b16 16) .&. w) `shiftR` 16)
    <> encodeWord16 (toInt $ b16 .&. w)

-- Same again, divide and conquer.
encodeWord64 :: Word64 -> ByteString
encodeWord64 w =
  encodeWord32 (toInt $ ((shiftL b32 32) .&. w) `shiftR` 32)
    <> encodeWord32 (toInt $ b32 .&. w)

encode :: Cbor -> Either String ByteString
encode (CUnsigned w) = pure $ encodeUnsigned w
encode (CNegative w) = pure $ encodeNegative w
encode (CByteString s) = pure $ encodeByteString s
encode (CText s) = pure $ encodeText s
encode (CArray l) = encodeArray l
encode (CMap m) = encodeMap m
encode (CTag t c) = encodeTag t c
encode CFalse = pure encodeFalse
encode CTrue = pure encodeTrue
encode CNull = pure encodeNull
encode CUndefined = pure encodeUndefined
encode (CHalf h) = pure $ encodeHalf h
encode (CFloat f) = pure $ encodeFloat f
encode (CDouble d) = pure $ encodeDouble d
encode (CSimple w) = encodeSimple w

encodeType :: Word8 -> Word8 -> Word8
encodeType m s = shiftL m 5 .|. (minorBits .&. s)

encodeUnsigned :: Word64 -> ByteString
encodeUnsigned w
  | w <= 23 = [encodeType majorType $ toInt w]
  | w <= b8 = [encodeType majorType 24] <> encodeWord8 (toInt w)
  | w <= b16 = [encodeType majorType 25] <> encodeWord16 (toInt w)
  | w <= b32 = [encodeType majorType 26] <> encodeWord32 (toInt w)
  | otherwise = [encodeType majorType 27] <> encodeWord64 (toInt w)
  where
    majorType = 0

encodeNegative :: Word64 -> ByteString
encodeNegative w
  | w' <= 23 = [encodeType majorType $ toInt w']
  | w' <= b8 = [encodeType majorType 24] <> encodeWord8 (toInt w')
  | w' <= b16 = [encodeType majorType 25] <> encodeWord16 (toInt w')
  | w' <= b32 = [encodeType majorType 26] <> encodeWord32 (toInt w')
  | otherwise = [encodeType majorType 27] <> encodeWord64 (toInt w')
  where
    majorType = 1
    w' = w - 1

encodeByteString :: ByteString -> ByteString
encodeByteString s
  | len <= 23 = [encodeType majorType $ toInt len] <> s
  | len <= b8 = [encodeType majorType 24] <> encodeWord8 (toInt len) <> s
  | len <= b16 = [encodeType majorType 25] <> encodeWord16 (toInt len) <> s
  | len <= b32 = [encodeType majorType 26] <> encodeWord32 (toInt len) <> s
  | otherwise = [encodeType majorType 27] <> encodeWord64 (toInt len) <> s
  where
    majorType = 2
    len = BS.length s

encodeText :: Text -> ByteString
encodeText t
  | len <= 23 = [encodeType majorType $ toInt len] <> s
  | len <= b8 = [encodeType majorType 24] <> encodeWord8 (toInt len) <> s
  | len <= b16 = [encodeType majorType 25] <> encodeWord16 (toInt len) <> s
  | len <= b32 = [encodeType majorType 26] <> encodeWord32 (toInt len) <> s
  | otherwise = [encodeType majorType 27] <> encodeWord64 (toInt len) <> s
  where
    majorType = 3
    s = encodeUtf8 t
    len = BS.length s

encodeArray :: [Cbor] -> Either String ByteString
encodeArray l
  | len <= 23 = go <&> \s -> [encodeType majorType $ toInt len] <> s
  | len <= b8 = go <&> \s -> [encodeType majorType 24] <> encodeWord8 (toInt len) <> s
  | len <= b16 = go <&> \s -> [encodeType majorType 25] <> encodeWord16 (toInt len) <> s
  | len <= b32 = go <&> \s -> [encodeType majorType 26] <> encodeWord32 (toInt len) <> s
  | otherwise = go <&> \s -> [encodeType majorType 27] <> encodeWord64 (toInt len) <> s
  where
    go :: Either String ByteString
    go = foldr f (pure mempty) l
    f a b = do
      a' <- encode a
      b' <- b
      pure $ a' <> b'
    majorType = 4
    len = length l

encodeMap :: Map Cbor Cbor -> Either String ByteString
encodeMap m
  | len <= 23 = go <&> \s -> [encodeType majorType $ toInt len] <> s
  | len <= b8 = go <&> \s -> [encodeType majorType 24] <> encodeWord8 (toInt len) <> s
  | len <= b16 = go <&> \s -> [encodeType majorType 25] <> encodeWord16 (toInt len) <> s
  | len <= b32 = go <&> \s -> [encodeType majorType 26] <> encodeWord32 (toInt len) <> s
  | otherwise = go <&> \s -> [encodeType majorType 27] <> encodeWord64 (toInt len) <> s
  where
    go = mconcat . sort <$> foldr f (pure mempty) l
    f (a, b) z = do
      a' <- encode a
      b' <- encode b
      z' <- z
      pure $ (a' <> b') : z'
    majorType = 5
    len = length l
    l = M.toList m

encodeTag :: Word64 -> Cbor -> Either String ByteString
encodeTag t c
  | t <= 23 = go <&> \s -> [encodeType majorType $ toInt t] <> s
  | t <= b8 = go <&> \s -> [encodeType majorType 24] <> encodeWord8 (toInt t) <> s
  | t <= b16 = go <&> \s -> [encodeType majorType 25] <> encodeWord16 (toInt t) <> s
  | t <= b32 = go <&> \s -> [encodeType majorType 26] <> encodeWord32 (toInt t) <> s
  | otherwise = go <&> \s -> [encodeType majorType 27] <> encodeWord64 (toInt t) <> s
  where
    majorType = 6
    go = encode c

encodeFalse :: ByteString
encodeFalse = [encodeType majorType 20]
  where
    majorType = 7

encodeTrue :: ByteString
encodeTrue = [encodeType majorType 21]
  where
    majorType = 7

encodeBool :: Bool -> ByteString
encodeBool False = encodeFalse
encodeBool True = encodeTrue

encodeNull :: ByteString
encodeNull = [encodeType majorType 22]
  where
    majorType = 7

encodeUndefined :: ByteString
encodeUndefined = [encodeType majorType 23]
  where
    majorType = 7

encodeHalf :: Half -> ByteString
encodeHalf h = [encodeType majorType 25] <> encodeWord16 (coerce $ getHalf h)
  where
    majorType = 7

encodeFloat :: Float -> ByteString
encodeFloat f =
  [encodeType majorType 26] <> encodeWord32 (castFloatToWord32 f)
  where
    majorType = 7

encodeDouble :: Double -> ByteString
encodeDouble d =
  [encodeType majorType 27] <> encodeWord64 (castDoubleToWord64 d)
  where
    majorType = 7

-- This does allow encoding of Bools, null, and undefined
-- But users should probably use the stricter types for those
-- as it will not round-trip parse to the same structure.
encodeSimple :: Word8 -> Either String ByteString
encodeSimple w
  | w >= 32 = pure $ [encodeType majorType 24] <> encodeWord8 w
  | w <= 23 = pure [encodeType majorType w]
  | otherwise = Left "Cannot encode a simple value in the range of [24..31]"
  where
      majorType = 7
