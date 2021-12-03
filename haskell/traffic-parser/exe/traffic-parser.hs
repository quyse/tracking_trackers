{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ViewPatterns #-}

module Main(main) where

import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import Data.Tuple
import qualified Options.Applicative as O
import qualified Text.XML.Stream.Parse as X

import qualified AhoCorasic as AC
import Parser
import SecretExpansion
import Types

main :: IO ()
main = run =<< O.execParser parser where
  parser = O.info (O.helper <*> opts)
    (  O.fullDesc
    <> O.progDesc "Tracking_Trackers traffic parser"
    <> O.header "traffic-parser"
    )
  opts = Options
    <$> O.strOption
      (  O.long "secrets-file"
      <> O.metavar "SECRETS_FILE"
      <> O.help "Secrets JSON file"
      )

data Options = Options
  { options_secretsFile :: FilePath
  }

run :: Options -> IO ()
run Options
  { options_secretsFile = secretsFile
  } = do
  (HM.fromList . map swap . concatMap expandSecret . HM.toList -> secrets) <- either fail return =<< J.eitherDecodeFileStrict secretsFile
  C.runConduit $ C.stdin .| X.parseBytes def .| parsePdml .| requestParser (AC.forStrings secrets) .| jsonArrayEncoder .| C.stdout

requestParser :: Monad m => AC.Automaton a -> C.ConduitT Request Request m ()
requestParser (((not . null) .) . AC.search -> search) = C.awaitForever $ \request -> let
  match = case request of
    Request_http
      { request_headers = Headers headers
      , request_body = body
      } -> or (search . T.encodeUtf8 <$> headers) || maybe False search body
    Request_http2
      { request_headers = Headers headers
      , request_body = body
      } -> or (search . T.encodeUtf8 <$> headers) || maybe False search body
  in when match $ C.yield request

jsonArrayEncoder :: (Monad m, J.ToJSON a) => C.ConduitT a B.ByteString m ()
jsonArrayEncoder = do
  C.yield "["
  traverse_ encodeItem =<< C.await
  C.awaitForever $ \item -> do
    C.yield ","
    encodeItem item
  C.yield "]"
  where
    encodeItem = mapM_ C.yield . BL.toChunks . J.encode
