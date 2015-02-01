module Graph (
    Vertex,
    Weight,
    Graph,
    weight
) where

import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap)

type Vertex = Int
type Weight = Int

type Graph = IntMap (IntMap Weight)

weight :: Graph -> Vertex -> Vertex -> Maybe Weight
weight g i j = do 
    jmap <- Map.lookup i g
    Map.lookup j jmap
