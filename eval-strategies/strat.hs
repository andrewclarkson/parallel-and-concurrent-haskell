import Control.Parallel.Strategies hiding (parPair)

fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)
 
parPair :: Strategy (a, b)
parPair (a, b) = do
    a' <- rpar a
    b' <- rpar b
    return (a', b')

main :: IO ()
main = do
    let pair = (fib 35, fib 36) `using` parPair
    print(pair)
    return ()

