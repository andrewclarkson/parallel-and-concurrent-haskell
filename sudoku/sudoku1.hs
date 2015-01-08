import Sudoku (solve)
import System.Environment (getArgs)
import Data.Maybe (isJust)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f
    
    let puzzles = lines file
        solutions = map solve puzzles 

    print $ length $ filter isJust solutions
