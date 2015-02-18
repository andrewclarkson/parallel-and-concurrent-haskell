import Data.Array.Repa
import qualified Data.Array.Repa as Repa

main :: IO ()
main = do
    let y = fromListUnboxed ((Z :. 3 :. 3 :. 2) :: DIM3) [1..18]
        y' = Repa.zipWith (*) y y
    result <- (computeP y') :: IO (Array U DIM3 Int)
    print result
