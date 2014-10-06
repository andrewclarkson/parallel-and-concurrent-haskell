import Control.Parallel.Strategies

totallyParallel :: (a -> b) -> a -> a -> (b, b)
totallyParallel f x y = runEval $ do
                          a <- rpar (f x)
                          b <- rpar (f y)
                          return (a, b)

main :: IO ()
main = do
    let tuple = totallyParallel show 6 7
    print(tuple)
    return ()
