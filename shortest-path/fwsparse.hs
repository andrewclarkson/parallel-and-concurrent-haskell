import Data.List (foldl')

import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap)

import Graph

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
    where
        update g k = Map.mapWithKey shortmap g
            where
                shortmap :: Vertex -> IntMap Weight -> IntMap Weight
                shortmap i jmap = foldr shortest Map.empty vs 
                    where
                        shortest j m = case (old, new) of
                            (Nothing, Nothing) -> m
                            (Nothing, Just w ) -> Map.insert j w m 
                            (Just w , Nothing) -> Map.insert j w m 
                            (Just w1, Just w2) -> Map.insert j (min w1 w2) m
                            where
                                old = Map.lookup j jmap
                                new = do 
                                    w1 <- weight g i k
                                    w2 <- weight g k j 
                                    return (w1 + w2) 
        
test :: [[Int]]
test  = [[  0, 999, 999,  13, 999, 999],
         [999,   0, 999, 999,   4,   9],
         [ 11, 999,   0, 999, 999, 999],
         [999,   3, 999,   0, 999,   7],
         [ 15,   5, 999,   1,   0, 999],
         [ 11, 999, 999,  14, 999,   0]]

mkGraph :: [[Int]] -> Graph
mkGraph xss = Map.fromList (zipWith row [0..] xss)
  where
   row i xs = (i, Map.fromList [ (j, w) | (j,w) <- zip [0..] xs, w /= 100 ])

main :: IO ()
main = do
    let
        shortest = shortestPaths [1..5] (mkGraph test)
    print shortest
