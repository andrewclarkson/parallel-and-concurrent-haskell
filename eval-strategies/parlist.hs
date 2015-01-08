import Control.Parallel.Strategies hiding (parList, parMap, evalList)


evalList :: Strategy a -> Strategy [a]
evalList strategy [] = return []
evalList strategy (x:xs) = do
    x' <- strategy x
    xs' <- evalList strategy xs
    return (x':xs')

parList :: Strategy a -> Strategy [a]
parList strategy = evalList $ rparWith strategy

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq

fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

main :: IO ()
main = do 
    let list = parMap fib [20..30]
    print(list)
    return ()
