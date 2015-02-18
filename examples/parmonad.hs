import Control.Monad.Par (runPar, new, fork, put, get)

main :: IO ()
main = do
    let result = runPar $ do
        a <- new
        b <- new
        fork $ put a (fib 30)
        fork $ put b (fib 31)
        a' <- get a
        b' <- get b
        return (a', b')
    print result
    where
        fib :: Int -> Int
        fib 0 = 1
        fib 1 = 1
        fib x = fib (x - 1) + fib (x - 2)

    
