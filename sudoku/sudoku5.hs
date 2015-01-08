import Sudoku (solve)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (parList, rseq, using)
import System.Environment (getArgs)
import Data.Maybe (isJust)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f
    
    let puzzles  = lines file 
        solutions = map solve puzzles `using` parList rseq

    evaluate (length puzzles)
    print $ length $ filter isJust solutions
