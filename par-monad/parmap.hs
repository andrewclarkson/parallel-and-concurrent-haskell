import Control.Monad.Par (NFData, IVar, Par, runPar, fork, get, put, new)

fib :: Int  -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do
    i <- new
    fork $ do
        x <- p
        put i x
    return i

parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
    ibs <- mapM (spawn . f) as
    mapM get ibs

fibM :: Int -> Par Int
fibM x = do
    return (fib x)


main :: IO ()
main = do
    print $ runPar $ parMapM fibM [0 .. 20]
