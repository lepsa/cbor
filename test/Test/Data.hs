{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Data where

import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy    (toStrict)
import           Data.Cbor
import           Data.Cbor.Encoder
import           Numeric.Half
import qualified Numeric.IEEE            as IEEE
import           Test.Util
import           Control.Arrow           (second)


-- Test values from RFC8949, Appendix A
-- The format of the list is the CBOR value, the hex string, the expected bignum value, the expected negative value

rfcTestStringsBigNum  :: [(Cbor, ByteString, Maybe Integer, Maybe Integer)]
rfcTestStringsBigNum = (\(a, b, c, d) -> (a, fromHex b, c, d)) <$>
  [ (CTag 2 (CByteString (fromHex "010000000000000000")), "c249010000000000000000", pure 18446744073709551616, Nothing)
  , (CNegative 18446744073709551615, "3bffffffffffffffff", Nothing, pure (-18446744073709551616))
  , (CTag 3 (CByteString (fromHex "010000000000000000")), "c349010000000000000000", pure (-18446744073709551617), Nothing)
  , (CNegative 0, "20", Nothing, pure (-1))
  , (CNegative 9, "29", Nothing, pure (-10))
  , (CNegative 99, "3863", Nothing, pure (-100))
  , (CNegative 999, "3903e7", Nothing, pure (-1000))
  ]

rfcTestStrings :: [(Cbor, ByteString)]
rfcTestStrings = second fromHex <$>
  [ (CUnsigned 0, "00")
  , (CUnsigned 1, "01")
  , (CUnsigned 10, "0a")
  , (CUnsigned 23, "17")
  , (CUnsigned 24, "1818")
  , (CUnsigned 25, "1819")
  , (CUnsigned 100, "1864")
  , (CUnsigned 1000, "1903e8")
  , (CUnsigned 1000000, "1a000f4240")
  , (CUnsigned 1000000000000, "1b000000e8d4a51000")
  , (CUnsigned 18446744073709551615, "1bffffffffffffffff")
  , (CHalf 0.0, "f90000")
  , (CHalf (-0.0), "f98000")
  , (CHalf 1.0, "f93c00")
  , (CDouble 1.1, "fb3ff199999999999a")
  , (CHalf 1.5, "f93e00")
  , (CHalf 65504.0, "f97bff")
  , (CFloat 100000.0, "fa47c35000")
  , (CFloat 3.4028234663852886e+38, "fa7f7fffff")
  , (CDouble 1.0e+300, "fb7e37e43c8800759c")
  , (CHalf 5.960464477539063e-8, "f90001")
  , (CHalf 0.00006103515625, "f90400")
  , (CHalf (-4.0), "f9c400")
  , (CDouble (-4.1), "fbc010666666666666")
  , (CHalf POS_INF, "f97c00")
  , (CHalf QNaN, "f97e00")
  , (CHalf NEG_INF, "f9fc00")
  , (CFloat IEEE.infinity, "fa7f800000")
  , (CFloat IEEE.nan, "fa7fc00000")
  , (CFloat (-IEEE.infinity), "faff800000")
  , (CDouble IEEE.infinity, "fb7ff0000000000000")
  , (CDouble IEEE.nan, "fb7ff8000000000000")
  , (CDouble (-IEEE.infinity), "fbfff0000000000000")
  , (cFalse, "f4")
  , (cTrue, "f5")
  , (cNull, "f6")
  , (cUndefined, "f7")
  , (CSimple 16, "f0")
  , (CSimple 255, "f8ff")
  , (CTag 0 (CText "2013-03-21T20:04:00Z"), "c074323031332d30332d32315432303a30343a30305a")
  , (CTag 1 (CUnsigned 1363896240), "c11a514b67b0")
  , (CTag 1 (CDouble 1363896240.5), "c1fb41d452d9ec200000")
  , (CTag 23 (CByteString $ fromHex "01020304"), "d74401020304")
  , (CTag 24 (either (error "Failed encoding") (CByteString . toStrict . toLazyByteString) $ encode $ CText "IETF"), "d818456449455446")
  , (CTag 32 (CText "http://www.example.com"), "d82076687474703a2f2f7777772e6578616d706c652e636f6d")
  , (CByteString $ fromHex "", "40")
  , (CByteString $ fromHex "01020304", "4401020304")
  , (CText "", "60")
  , (CText "a", "6161")
  , (CText "IETF", "6449455446")
  , (CText "\"\\", "62225c")
  , (CText "\x00fc", "62c3bc")
  , (CText "\x6c34", "63e6b0b4")
  , (CText "\x10151", "64f0908591")
  , (CArray [], "80")
  , (CArray [CUnsigned 1, CUnsigned 2, CUnsigned 3], "83010203")
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "8301820203820405")
  , (CArray [CUnsigned 1, CUnsigned 2, CUnsigned 3, CUnsigned 4, CUnsigned 5, CUnsigned 6, CUnsigned 7, CUnsigned 8, CUnsigned 9, CUnsigned 10, CUnsigned 11, CUnsigned 12, CUnsigned 13, CUnsigned 14, CUnsigned 15, CUnsigned 16, CUnsigned 17, CUnsigned 18, CUnsigned 19, CUnsigned 20, CUnsigned 21, CUnsigned 22, CUnsigned 23, CUnsigned 24, CUnsigned 25], "98190102030405060708090a0b0c0d0e0f101112131415161718181819")
  , (CMap mempty, "a0")
  , (CMap [(CUnsigned 1, CUnsigned 2), (CUnsigned 3, CUnsigned 4)], "a201020304")
  , (CMap [(CText "a", CUnsigned 1), (CText "b", CArray [CUnsigned 2, CUnsigned 3])], "a26161016162820203")
  , (CArray [CText "a", CMap [(CText "b", CText "c")]] , "826161a161626163")
  , (CMap [(CText "a", CText "A"), (CText "b", CText "B"), (CText "c", CText "C"), (CText "d", CText "D"), (CText "e", CText "E")], "a56161614161626142616361436164614461656145")
  , (CByteStringStreaming [fromHex "0102", fromHex "030405"], "5f42010243030405ff")
  , (CTextStreaming ["strea", "ming"], "7f657374726561646d696e67ff")
  , (CArrayStreaming [], "9fff")
  , (CArrayStreaming [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArrayStreaming [CUnsigned 4, CUnsigned 5]], "9f018202039f0405ffff")
  , (CArrayStreaming [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "9f01820203820405ff")
  , (CArray [CUnsigned 1, CArray [CUnsigned 2, CUnsigned 3], CArrayStreaming [CUnsigned 4, CUnsigned 5]], "83018202039f0405ff")
  , (CArray [CUnsigned 1, CArrayStreaming [CUnsigned 2, CUnsigned 3], CArray [CUnsigned 4, CUnsigned 5]], "83019f0203ff820405")
  , (CArrayStreaming [CUnsigned 1, CUnsigned 2, CUnsigned 3, CUnsigned 4, CUnsigned 5, CUnsigned 6, CUnsigned 7, CUnsigned 8, CUnsigned 9, CUnsigned 10, CUnsigned 11, CUnsigned 12, CUnsigned 13, CUnsigned 14, CUnsigned 15, CUnsigned 16, CUnsigned 17, CUnsigned 18, CUnsigned 19, CUnsigned 20, CUnsigned 21, CUnsigned 22, CUnsigned 23, CUnsigned 24, CUnsigned 25], "9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff")
  , (CMapStreaming [(CText "a", CUnsigned 1), (CText "b", CArrayStreaming [CUnsigned 2, CUnsigned 3])], "bf61610161629f0203ffff")
  , (CArray [CText "a", CMapStreaming [(CText "b", CText "c")]], "826161bf61626163ff")
  , (CMapStreaming [(CText "Fun", cTrue), (CText "Amt", CNegative 1)], "bf6346756ef563416d7421ff")
  ]
