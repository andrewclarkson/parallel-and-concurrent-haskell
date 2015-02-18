import Control.Monad.Par (runPar, new, fork, put, get)

main :: IO ()
main = do
    let result = runPar $ do
        a <- new
        fork $ put a (fib 31)
        fork $ put a (fib 1)
        a' <- get a
        return a'
    print result
    where
        fib :: Int -> Int
        fib 0 = 1
        fib 1 = 1
        fib x = fib (x - 1) + fib (x - 2)

    
