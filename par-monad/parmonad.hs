import Control.Monad.Par (Par, runPar, fork, get, put, new)

fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

fibPar :: Int -> Int -> Par Int
fibPar n m = do 
    i <- new 
    j <- new
    fork (put i (fib n))
    fork (put j (fib m))
    a <- get i
    b <- get j
    return (a + b)

main :: IO ()
main = do
    print (runPar (fibPar 12 13))
