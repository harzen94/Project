module Main where

import Control.Monad (forever)
import Sudoku (getSudokuField, makeField, makeSudoku, printSudoku)
import System.Console.ANSI

main :: IO ()
main = do
  let testInput = "111111111222222222333333333444444444555555555666666666777777777888888888999999999"
  setTitle "SudokuSolver"
  setSGR [SetColor Foreground Dull Blue]
  let x = makeSudoku testInput
  printSudoku (makeSudoku testInput)
  setSGR [Reset]

--forever (getLine >>= putStrLn)
