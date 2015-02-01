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

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap f as = do
    ibs <- mapM (spawn . return . f) as
    mapM get ibs

main :: IO ()
main = do
    print $ runPar $ parMap fib [0 .. 20]
