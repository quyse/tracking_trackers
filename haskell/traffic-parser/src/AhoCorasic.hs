{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, ViewPatterns #-}

module AhoCorasic
  ( Automaton()
  , forStrings
  , search
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable
import Data.Tuple
import Data.Word

data Automaton a = Automaton
  { automaton_nodes :: !(HM.HashMap NodeId (Node a))
  , automaton_edges :: !(HM.HashMap EdgeId NodeId)
  }

-- | 24-bit node id.
newtype NodeId = NodeId Word32 deriving (Eq, Hashable)

-- | 8-bit symbol (byte).
newtype Symbol = Symbol Word8

-- | 32-bit edge id (node id + byte)
newtype EdgeId = EdgeId Word32 deriving (Eq, Hashable)

mkEdgeId :: NodeId -> Symbol -> EdgeId
mkEdgeId (NodeId nodeId) (Symbol symbol) = EdgeId ((nodeId `shiftL` 8) .|. fromIntegral symbol)

unEdgeId :: EdgeId -> (NodeId, Symbol)
unEdgeId (EdgeId edgeId) = (NodeId (edgeId `shiftR` 8), Symbol (fromIntegral $ edgeId .&. 0xFF))

rootNodeId :: NodeId
rootNodeId = NodeId 0

invalidNodeId :: NodeId
invalidNodeId = NodeId 0xFFFFFFFF

data Node a = Node
  { node_suffixNodeId :: {-# UNPACK #-} !NodeId
  , node_maybeValue :: !(Maybe a)
  }

forStrings :: HM.HashMap B.ByteString a -> Automaton a
forStrings strings = execState f Automaton
  { automaton_nodes = HM.singleton rootNodeId Node
    { node_suffixNodeId = invalidNodeId
    , node_maybeValue = Nothing
    }
  , automaton_edges = HM.empty
  }
  where
    f = do
      mapM_ addString $ HM.toList strings
      ensureLinks
    addString (string, value) = do
      nodeId <- ofoldM addByte rootNodeId string
      modify $ \automaton@Automaton
        { automaton_nodes = nodes
        } -> automaton
        { automaton_nodes = HM.adjust (\node -> node
          { node_maybeValue = Just value
          }) nodeId nodes
        }
      return nodeId
    addByte nodeId byte = do
      automaton@Automaton
        { automaton_nodes = nodes
        , automaton_edges = edges
        } <- get
      let
        edgeId = mkEdgeId nodeId (Symbol byte)
      case HM.lookup edgeId edges of
        Just nextNodeId -> return nextNodeId
        Nothing -> do
          let
            nextNodeId = NodeId $ fromIntegral $ HM.size nodes
          put automaton
            { automaton_nodes = HM.insert nextNodeId Node
              { node_suffixNodeId = invalidNodeId
              , node_maybeValue = Nothing
              } nodes
            , automaton_edges = HM.insert edgeId nextNodeId edges
            }
          return nextNodeId
    ensureLinks = do
      Automaton
        { automaton_edges = edges
        } <- get
      let
        parents = HM.fromList $ map swap $ HM.toList edges
        -- only for existing nodeId != 0
        getLink nodeId = do
          Automaton
            { automaton_nodes = (HM.! nodeId) -> Node
              { node_suffixNodeId = existingSuffixNodeId
              }
            } <- get
          if existingSuffixNodeId == invalidNodeId
            then do
              let
                (parentNodeId, parentSymbol) = unEdgeId $ parents HM.! nodeId
              suffixNodeId <- if parentNodeId == rootNodeId
                then return rootNodeId
                else advance parentSymbol =<< getLink parentNodeId
              modify $ \automaton@Automaton
                { automaton_nodes = nodes
                } -> automaton
                { automaton_nodes = HM.adjust (\node -> node
                  { node_suffixNodeId = suffixNodeId
                  }) nodeId nodes
                }
              return suffixNodeId
            else return existingSuffixNodeId
        advance symbol nodeId = case HM.lookup (mkEdgeId nodeId symbol) edges of
          Just nextNodeId -> return nextNodeId
          Nothing -> if nodeId == rootNodeId
            then return rootNodeId
            else advance symbol =<< getLink nodeId
      mapM_ (void . getLink) (HM.elems edges)

search :: Automaton a -> B.ByteString -> [a]
search Automaton
  { automaton_nodes = nodes
  , automaton_edges = edges
  } = reverse . snd . B.foldl' f (rootNodeId, [])
  where
    f (nodeId, matches) (Symbol -> symbol) = let
      nextNodeId = advance symbol nodeId
      in if nextNodeId == rootNodeId
        then (nextNodeId, matches)
        else
          ( nextNodeId
          , case nodes HM.! nextNodeId of
              Node
                { node_maybeValue = Just value
                } -> value : matches
              _ -> matches
          )
    advance symbol nodeId = case HM.lookup (mkEdgeId nodeId symbol) edges of
      Just nextNodeId -> nextNodeId
      Nothing -> if nodeId == rootNodeId
        then rootNodeId
        else advance symbol $ node_suffixNodeId $ nodes HM.! nodeId
