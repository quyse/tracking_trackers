{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types
  ( Request(..)
  , TcpStreamId(..)
  , Http2StreamId(..)
  , Headers(..)
  , Address(..)
  , Port(..)
  , SecretId(..)
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics(Generic)

data Request
  = Request_http
    { request_tcpStreamId :: !TcpStreamId
    , request_headers :: !Headers
    , request_body :: !(Maybe B.ByteString)
    }
  | Request_http2
    { request_tcpStreamId :: !TcpStreamId
    , request_http2StreamId :: !Http2StreamId
    , request_headers :: !Headers
    , request_body :: !(Maybe B.ByteString)
    }
  deriving Generic
instance J.ToJSON Request where
  toJSON = J.genericToJSON jsonOptions
  toEncoding = J.genericToEncoding jsonOptions

data TcpStreamId = TcpStreamId
  { tcpStreamId_src :: Address
  , tcpStreamId_dst :: Address
  , tcpStreamId_srcPort :: Port
  , tcpStreamId_dstPort :: Port
  } deriving (Eq, Generic)
instance Hashable TcpStreamId
instance J.ToJSON TcpStreamId where
  toJSON = J.genericToJSON jsonOptions
  toEncoding = J.genericToEncoding jsonOptions

newtype Http2StreamId = Http2StreamId T.Text deriving (Eq, Hashable, J.ToJSON)

newtype Headers = Headers (V.Vector T.Text) deriving J.ToJSON

newtype Address = Address T.Text deriving (Eq, Hashable, J.ToJSON)
newtype Port = Port T.Text deriving (Eq, Hashable, J.ToJSON)

newtype SecretId = SecretId T.Text deriving (Eq, Hashable, J.FromJSONKey)

instance J.ToJSON B.ByteString where
  toJSON = J.toJSON . T.decodeUtf8 . BA.convertToBase BA.Base16
  toEncoding = J.toEncoding . T.decodeUtf8 . BA.convertToBase BA.Base16

jsonOptions :: J.Options
jsonOptions = jsonOptionsWithTag "type"

jsonOptionsWithTag :: String -> J.Options
jsonOptionsWithTag tag = J.defaultOptions
  { J.fieldLabelModifier = dropBeforeUnderscore
  , J.constructorTagModifier = dropBeforeUnderscore
  , J.sumEncoding = J.TaggedObject
    { J.tagFieldName = tag
    , J.contentsFieldName = "contents"
    }
  }

dropBeforeUnderscore :: String -> String
dropBeforeUnderscore = \case
  x : xs -> case x of
    '_' -> xs
    _ -> dropBeforeUnderscore xs
  [] -> []
