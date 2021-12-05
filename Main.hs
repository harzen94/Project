module Main where

import Control.Monad (forever)
import Sudoku (getSudokuField, makeField, makeSudoku, printSudoku)

main :: IO ()
main = do
  let testInput = "111111111222222222333333333444444444555555555666666666777777777888888888999999999"
  let x = makeSudoku testInput
  printSudoku (makeSudoku testInput)

--forever (getLine >>= putStrLn)
