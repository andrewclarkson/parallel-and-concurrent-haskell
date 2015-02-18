import Control.Concurrent

main :: IO ()
main = do
    a <- newEmptyMVar
    forkIO $ do
        putMVar a (fib 31)
    forkIO $ do
        putMVar a (fib 30)
    result <- takeMVar a
    print result
    where
        fib :: Int -> Int
        fib 0 = 1
        fib 1 = 1
        fib x = fib (x - 1) + fib (x - 2)

    
