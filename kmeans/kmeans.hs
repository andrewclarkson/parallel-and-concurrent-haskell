import System.IO (readFile)
import Data.Binary
import Text.Printf (printf)
import System.Mem (performGC)
import Control.Concurrent (runInUnboundThread)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Function (on)
import Data.Binary (decodeFile)
import Data.List (minimumBy)
import Control.Monad.Primitive (PrimState, PrimMonad)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MVector
import Data.Vector.Mutable (MVector)


-- The "!" in "!Double" means that these types are not
-- lazy and thus don't have the overhead of thunks. 

data Point = Point !Double !Double deriving (Show, Eq, Read)

instance Binary Point where
  put (Point a b) = put a >> put b
  get = do a <- get; b <- get; return (Point a b)

zeroPoint :: Point
zeroPoint = Point 0 0 

-- Computes the distance between two points
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

data Cluster = Cluster {cid :: Int, centroid :: Point} deriving (Show, Eq, Read)

-- This data structure keeps partial sums to calculate
-- the centroid
data PointSum = PointSum !Int !Double !Double

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y)
    = PointSum (count + 1) (xs + x) (ys + y)

-- A centroid is the average of the points this 
-- function calculates the centroid from the PointSum
calculateCentroid :: PointSum -> Point
calculateCentroid (PointSum count xs ys)
    = Point (xs / fromIntegral count) (ys / fromIntegral count)

-- Creates a cluster from a PointSum and identifier
createCluster :: Int -> PointSum -> Cluster
createCluster cid pointSum =
    Cluster { cid = cid, centroid = calculateCentroid pointSum }

-- finds the index of the nearest cluster to the point
nearest :: [Cluster] -> Point -> Cluster
nearest clusters point = fst $ minimumBy (compare `on` snd) 
    [(cluster, distance (centroid cluster) point) | cluster <- clusters] 

-- Adds a point to the cluster at cid
addPoint :: PrimMonad m => MVector (PrimState m) PointSum -> (Point -> Cluster) -> Point -> m ()
addPoint vector nearest point = do
    let index = cid (nearest point)
    pointSum <- MVector.read vector index
    MVector.write vector index $! addToPointSum pointSum point


-- Assigns points to the cluster they are closest to
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign length clusters points = Vector.create $ do
    -- Create an empty mutable vector (MVector) of PointSums
    vector <- MVector.replicate length (PointSum 0 0 0)
    mapM_ (addPoint vector (nearest clusters)) points
    return vector

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vector = 
    [createCluster index pointSum
    | (index, pointSum@(PointSum count _ _)) <- zip [0..] (Vector.toList vector)
    , count > 0
    ]

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step length clusters points = makeNewClusters (assign length clusters points)

kmeans :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans nclusters points clusters = 
    let
        loop :: Int -> [Cluster] -> IO [Cluster]
        loop n clusters | n > tooMany = do
            putStrLn "Too many iterations have passed"
            return clusters
        loop n clusters = do
            printf "Iteration %d\n" n
            putStr (unlines (map show clusters))
            let clusters' = step nclusters clusters points
            if clusters' == clusters
                then return clusters
                else loop (n + 1) clusters'
    in
    loop 0 clusters

tooMany = 80

main :: IO ()
main = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read `fmap` readFile "clusters"
  let nclusters = length clusters
  performGC
  t0 <- getCurrentTime
  final_clusters <- kmeans nclusters points clusters
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

