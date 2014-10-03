import Control.Parallel.Strategies

parallelAndWait :: (a -> b) -> a -> a -> (b, b)
parallelAndWait f x y = runEval $ do
                          a <- rpar (f x)
                          b <- rseq (f y)
                          rseq a
                          return (a, b)

main :: IO ()
main = do
    let tuple = parallelAndWait show 6 7
    print(tuple)
    return ()
