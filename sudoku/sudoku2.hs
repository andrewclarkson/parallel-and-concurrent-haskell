import Sudoku (solve)
import Control.Parallel.Strategies (runEval, rpar, rseq)
import System.Environment (getArgs)
import Control.DeepSeq (force)
import Data.Maybe (isJust)

main :: IO ()
main = do 
    [f] <- getArgs
    file <- readFile f
    
    let puzzles   = lines file 
        (as,bs)   = splitAt half puzzles where half = length puzzles `div` 2
        solutions = runEval $ do
            as' <- rpar $ force $ map solve as 
            bs' <- rpar $ force $ map solve bs
            rseq as'
            rseq bs'
            return (as' ++ bs')
 
    print $ length $ filter isJust solutions
