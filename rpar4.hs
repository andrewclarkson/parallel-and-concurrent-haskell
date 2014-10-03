import Control.Parallel.Strategies

waitForBoth :: (a -> b) -> a -> a -> (b, b)
waitForBoth f x y = runEval $ do
                          a <- rpar (f x)
                          b <- rpar (f y)
                          rseq a
                          rseq b
                          return (a, b)

main :: IO ()
main = do
    let tuple = waitForBoth show 6 7
    print(tuple)
    return ()
