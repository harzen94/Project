module Main where

import Control.Monad (forever)
import Sudoku (getSudokuField)
import System.Console.ANSI
  ( Color (Blue),
    ColorIntensity (Dull),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGR,
    setTitle,
  )

main :: IO ()
main = do
  setTitle "SudokuSolver"
  setSGR [SetColor Foreground Dull Blue]
  putStrLn ("\n\n\n" ++ getSudokuField [[1, 2, 3], [3, 2, 1], [4, 5, 6]] "" ++ "\n\n\n")
  setSGR [Reset]
  forever (getLine >>= putStrLn)
