{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parser
  ( parsePdml
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict(StateT, get, modify)
import Control.Monad.Trans.Class
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Lift as C
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.XML.Types as X
import qualified Text.XML.Stream.Parse as X

import Types

parsePdml :: MonadThrow m => C.ConduitT X.Event Request m ()
parsePdml = C.evalStateC TrafficState
  { traffic_http2 = mempty
  } $ X.force "pdml tag required" $ X.tagIgnoreAttrs "pdml" $ X.many_ parsePacket

parsePacket :: MonadThrow m => C.ConduitT X.Event Request (StateT TrafficState m) (Maybe ())
parsePacket = C.evalStateC PacketState
  { packet_tcpStreamId = TcpStreamId
    { tcpStreamId_src = Address ""
    , tcpStreamId_dst = Address ""
    , tcpStreamId_srcPort = Port ""
    , tcpStreamId_dstPort = Port ""
    }
  , packet_http2StreamId = Http2StreamId mempty
  , packet_headers = mempty
  , packet_body = Nothing
  , packet_httpEndStream = False
  } $ X.tagNoAttr "packet" $ X.many_ parseProto

parseProto :: MonadThrow m => C.ConduitT X.Event Request (StateT PacketState (StateT TrafficState m)) (Maybe ())
parseProto = X.tag' "proto" parseAttrs $ \name -> do
  X.many_ $ parseField name
  X.many_ parseProto
  case name of
    "http" -> do
      PacketState
        { packet_tcpStreamId = tcpStreamId
        , packet_headers = headers
        , packet_body = body
        } <- get
      C.yield Request_http
        { request_tcpStreamId = tcpStreamId
        , request_headers = Headers $ V.reverse $ V.fromList headers
        , request_body = body
        }
    "http2" -> do
      -- get stream id
      s2@PacketState
        { packet_tcpStreamId = tcpStreamId
        , packet_http2StreamId = http2StreamId
        } <- get
      let
        streamId = (tcpStreamId, http2StreamId)
      -- merge or record stream info
      lift $ lift $ modify $ \ts -> ts
        { traffic_http2 = HM.alter (\case
          Just s -> Just s
            { packet_headers = packet_headers s2 ++ packet_headers s
            , packet_body = maybe (packet_body s) Just (packet_body s2)
            , packet_httpEndStream = packet_httpEndStream s || packet_httpEndStream s2
            }
          Nothing -> Just s2
          ) streamId (traffic_http2 ts)
        }
      -- fetch (possibly merged) stream
      PacketState
        { packet_headers = headers
        , packet_body = body
        , packet_httpEndStream = httpEndStream
        } <- (HM.! streamId) . traffic_http2 <$> (lift $ lift get)
      -- if finished
      when httpEndStream $ do
        -- remove stream
        lift $ lift $ modify $ \s -> s
          { traffic_http2 = HM.delete streamId (traffic_http2 s)
          }
        -- yield request
        C.yield Request_http2
          { request_tcpStreamId = tcpStreamId
          , request_http2StreamId = http2StreamId
          , request_headers = Headers $ V.reverse $ V.fromList headers
          , request_body = body
          }
    _ -> return ()
  where
    parseAttrs = do
      name <- X.requireAttr "name"
      X.ignoreAttrs
      return name

parseField :: MonadThrow m => T.Text -> C.ConduitT X.Event Request (StateT PacketState (StateT TrafficState m)) (Maybe ())
parseField proto = X.tag' "field" parseAttrs $ \(name, maybeValue, maybeShow, maybeShowName) -> do
  let
    addLineFromValue = do
      line <- fromHex $ fromJust maybeValue
      modify $ \s -> s
        { packet_headers = T.decodeUtf8 line : packet_headers s
        }
    addBody = do
      value <- fromHex $ fromJust maybeValue
      modify $ \s -> s
        { packet_body = Just value
        }
  case (proto, name) of
    ("ip", "ip.src") -> modify $ \s -> s
      { packet_tcpStreamId = (packet_tcpStreamId s)
        { tcpStreamId_src = Address $ fromJust maybeShow
        }
      }
    ("ip", "ip.dst") -> modify $ \s -> s
      { packet_tcpStreamId = (packet_tcpStreamId s)
        { tcpStreamId_dst = Address $ fromJust maybeShow
        }
      }
    ("tcp", "tcp.srcport") -> modify $ \s -> s
      { packet_tcpStreamId = (packet_tcpStreamId s)
        { tcpStreamId_srcPort = Port $ fromJust maybeShow
        }
      }
    ("tcp", "tcp.dstport") -> modify $ \s -> s
      { packet_tcpStreamId = (packet_tcpStreamId s)
        { tcpStreamId_dstPort = Port $ fromJust maybeShow
        }
      }
    ("http", "") -> addLineFromValue
    ("http", "http.request.line") -> addLineFromValue
    ("http", "http.file_data") -> addBody
    ("http2", "http2.streamid") -> modify $ \s -> s
      { packet_http2StreamId = Http2StreamId $ fromJust maybeValue
      }
    ("http2", "http2.header") -> modify $ \s -> s
      { packet_headers = let
        header = fromJust maybeShowName
        in fromMaybe header (T.stripPrefix "Header: " header) : packet_headers s
      }
    ("http2", "http2.data.data") -> addBody
    ("http2", "http2.flags.end_stream") -> modify $ \s -> s
      { packet_httpEndStream = maybeValue == Just "1"
      }
    _ -> return ()
  X.many_ $ parseField proto
  X.many_ parseProto
  where
    parseAttrs = do
      name <- X.requireAttr "name"
      maybeValue <- X.attr "value"
      maybeShow <- X.attr "show"
      maybeShowName <- X.attr "showname"
      X.ignoreAttrs
      return (name, maybeValue, maybeShow, maybeShowName)

data TrafficState = TrafficState
  { traffic_http2 :: !(HM.HashMap (TcpStreamId, Http2StreamId) PacketState)
  }

data PacketState = PacketState
  { packet_tcpStreamId :: !TcpStreamId
  , packet_http2StreamId :: !Http2StreamId
  , packet_headers :: ![T.Text]
  , packet_body :: !(Maybe B.ByteString)
  , packet_httpEndStream :: !Bool
  }

fromHex :: MonadThrow m => T.Text -> m B.ByteString
fromHex = either (throwM . FormatException) return . BA.convertFromBase BA.Base16 . T.encodeUtf8 

newtype FormatException = FormatException String deriving Show
instance Exception FormatException
