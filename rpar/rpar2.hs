import Control.Parallel.Strategies

halfAndHalf :: (a -> b) -> a -> a -> (b, b)
halfAndHalf f x y = runEval $ do
                          a <- rpar (f x)
                          b <- rseq (f y)
                          return (a, b)

main :: IO ()
main = do
    let tuple = halfAndHalf show 6 7
    print(tuple)
    return ()
