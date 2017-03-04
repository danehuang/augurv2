{- 
 - Copyright 2017 Daniel Eachern Huang
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -    http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module Core.Graph where

import qualified Data.Graph as G
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.PrettyPrint

import AstUtil.Pretty
import AstUtil.Var

    
----------------------------------------------------------------------
-- = Graph Description
{-| [Note]

Wrapper for Data.Graph that works directly on nodes instead of keys/vertices.

(Value) node <-> key <-> vertex (Identifier)

-}



-----------------------------------
-- == Types

data Graph node = Graph { unGraph :: G.Graph
                        , keyNodeM :: Map.Map Key node
                        , vertM :: G.Vertex -> (node, Key, [Key])
                        , keyVertM :: Key -> Maybe G.Vertex }

newtype Key = Key { unKey :: Int}
    deriving (Show, Eq, Ord)
             
class (Pretty node) => Node node where
    toKey :: node -> Key

    -- toKey' :: node -> G.Vertex
    -- toKey' = unKey . toKey

             

-----------------------------------
-- == Helper
             
splatAdjLs :: (Ord key) => [(key, key)] -> [(key, [key])]
splatAdjLs adjLs =
    let adjM = foldl (\acc (k1, k2) ->
                          Map.insert k1 Set.empty (Map.insert k2 Set.empty acc)
                     ) Map.empty adjLs
        adjM' = foldl (\acc (k1, k2) ->
                           case Map.lookup k1 acc of
                             Just adj -> Map.insert k1 (Set.insert k2 adj) acc
                             Nothing -> Map.insert k1 (Set.singleton k2) acc
                      ) adjM adjLs
    in
      map (\(k, adj) -> (k, Set.toList adj)) (Map.toList adjM')

          
unsplatAdjLs :: (Ord key) => [(key, [key])] -> [(key, key)]
unsplatAdjLs adjLs =
    concat (map (\(k1, adj) -> map (\k2 -> (k1, k2)) adj) adjLs)


keyToVert :: Graph node -> Key -> G.Vertex
keyToVert graph k =
    case (keyVertM graph) k of
      Just v -> v
      Nothing -> error $ "[Graph] | @keyToVert | Key does not exist: " ++ pprShow k
           
vertToNode :: (Node node) => Graph node -> G.Vertex -> node
vertToNode graph v = let (n, _, _) = (vertM graph) v in n

keyToNode :: (Node node) => Graph node -> Key -> node
keyToNode graph = vertToNode graph . keyToVert graph
                  
vertToKey :: (Node node) => Graph node -> G.Vertex -> Key
vertToKey graph v = let (_, k, _) = (vertM graph) v in k

vertToAdj :: (Node node) => Graph node -> G.Vertex -> [Key]
vertToAdj graph v = let (_, _, adj) = (vertM graph) v in adj
                                    
nodeToVert :: (Node node) => Graph node -> node -> Maybe G.Vertex
nodeToVert graph n = (keyVertM graph) (toKey n)
                                          

-----------------------------------
-- == Interface


-- === Building

mkGraph :: (Node node) => [(node, node)] -> Graph node
mkGraph adjLs =
    let keyNodeM' = foldl (\acc (n1, n2) ->
                               Map.insert (toKey n1) n1
                                      (Map.insert (toKey n2) n2 acc)
                          ) Map.empty adjLs
        adjKeys = splatAdjLs (map (\(n1, n2) -> (toKey n1, toKey n2)) adjLs)
        adjKeys' = map (\(k, adj) ->
                            (fromJust (Map.lookup k keyNodeM'), k, adj)
                       ) adjKeys
        (graph, vertM', keyVertM') = G.graphFromEdges adjKeys'
    in
      Graph graph keyNodeM' vertM' keyVertM'


-- === Operations on whole graph
            
vertices :: (Node node) => Graph node -> [node]
vertices graph =
    map (vertToNode graph) (G.vertices (unGraph graph))
          
          
edges :: (Node node) => Graph node -> [(node, node)]
edges graph =
    map (\(v1, v2) -> ( vertToNode graph v1
                      , vertToNode graph v2)
        ) (G.edges (unGraph graph))

    
topSort :: (Node node) => Graph node -> [node]
topSort graph =
    let verts = G.topSort (unGraph graph)
        nodes = map (\v -> let (n, _, _) = (vertM graph) v in n) verts
    in
      nodes

      
-- === Operations on individual nodes

path :: (Node node) => Graph node -> node -> node -> Bool
path graph n1 n2 =
    case (nodeToVert graph n1, nodeToVert graph n2) of
      (Just v1, Just v2) ->
          G.path (unGraph graph) v1 v2
      _ -> False

           
reachable :: (Node node) => Graph node -> node -> [node]
reachable graph n =
    case nodeToVert graph n of
      Just v -> map (vertToNode graph) (G.reachable (unGraph graph) v)
      Nothing -> []

    
verticesOf :: (Node node) => Graph node -> node -> [node]
verticesOf graph n =
    case nodeToVert graph n of
      Just v -> map (keyToNode graph) ((vertToAdj graph) v)
      Nothing -> []
    
      
                   

          
-----------------------------------
-- == Instances

instance (Node node, Pretty node) => Pretty (Graph node) where
    ppr graph =
        text "Edges:" <+> brackets (sepBy' commasp (\(n1, n2) -> ppr n1 <+> text "->" <+> ppr n2) (edges graph))
             
    
instance Pretty Key where
    ppr (Key i) = int i
      

instance Node (TVar t) where
    toKey x = Key (getUid x)
