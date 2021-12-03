{-# LANGUAGE OverloadedStrings, TupleSections #-}

module SecretExpansion
  ( expandSecret
  ) where

import Control.Arrow
import qualified Crypto.Hash as C
import Data.Bits
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

expandSecret :: (T.Text, T.Text) -> [(T.Text, B.ByteString)]
expandSecret (id0, text0) = do
  -- various case conversions
  (id1, text1) <- flip map
    [ ("", id)
    , ("_L", T.toLower)
    , ("_U", T.toUpper)
    , ("_F", T.toCaseFold)
    ] $ \(p, f) -> (id0 <> p, f text0)

  -- encode to bytes with different encodings
  pair2 <- flip map
    [ T.encodeUtf8
    , T.encodeUtf16LE
    , T.encodeUtf16BE
    ] $ \f -> (id1, f text1)

  -- add hashes
  (id3, bytes3) <- let
    hash :: C.HashAlgorithm a => (T.Text, Proxy a) -> (T.Text, B.ByteString) -> (T.Text, B.ByteString)
    hash (s, p) = (<> s) *** (BA.convert . f p) where
      f :: C.HashAlgorithm a => Proxy a -> B.ByteString -> C.Digest a
      f Proxy = C.hash
    in map ($ pair2) $ id :
      [ hash ("_md5", Proxy :: Proxy C.MD5)
      , hash ("_sha1", Proxy :: Proxy C.SHA1)
      , hash ("_sha256", Proxy :: Proxy C.SHA256)
      ]

  -- add base64 encodings
  let
    -- convert to base64, expanded with various ending bytes
    base64 :: B.ByteString -> [B.ByteString]
    base64 bs = let
      in case B.length bs `rem` 3 of
        0 -> [BA.convertToBase BA.Base64 bs]
        1 -> [skipLast 2 $ BA.convertToBase BA.Base64 $ bs `B.snoc` (b `shiftL` 4) | b <- [0..15]]
        2 -> [skipLast 1 $ BA.convertToBase BA.Base64 $ bs `B.snoc` (b `shiftL` 6) | b <- [0..3]]
        _ -> error "base64 encoding: impossible"

    base64Offset2 :: B.ByteString -> [B.ByteString]
    base64Offset2 bs = do
      b <- [0..3]
      B.drop 1 <$> base64 (b `B.cons` bs)

    base64Offset4 :: B.ByteString -> [B.ByteString]
    base64Offset4 bs = do
      b <- [0..15]
      B.drop 2 <$> base64 (B.pack [0, b] <> bs)

    in (id3, ) <$> (bytes3 : concat [base64 bytes3, base64Offset2 bytes3, base64Offset4 bytes3])

skipLast :: Int -> B.ByteString -> B.ByteString
skipLast n bs = B.take (B.length bs - n) bs
