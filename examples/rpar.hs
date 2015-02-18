import Control.Parallel.Strategies (runEval, rpar)

main :: IO ()
main = do
    let result = runEval $ do
        a <- rpar $ fib 30
        b <- rpar $ fib 31
        return (a, b)
    print result
    where
        fib 0 = 1
        fib 1 = 1
        fib x = fib (x - 1) + fib (x - 2)

    
