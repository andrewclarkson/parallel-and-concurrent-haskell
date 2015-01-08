import Sudoku (solve)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (Eval, runEval, rpar)
import System.Environment (getArgs)
import Data.Maybe (isJust)

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
    b <- rpar $ f a
    bs <- parMap f as
    return (b:bs)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f
    
    let puzzles  = lines file 
        solutions = runEval $ parMap solve puzzles

    evaluate (length puzzles)
    print $ length $ filter isJust solutions
