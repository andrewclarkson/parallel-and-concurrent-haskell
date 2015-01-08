import Control.Parallel.Strategies hiding (parPair)

fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
evalPair sa sb (a,b) = do
    a' <- sa a
    b' <- sb b
    return (a', b')
 
parPair :: Strategy (a, b)
parPair = evalPair rpar rpar

main :: IO ()
main = do
    let pair = (fib 35, fib 36) `using` parPair
    print(pair)
    return ()

