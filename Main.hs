module Main where

import Sudoku (Sudoku, istValide)

easy :: Sudoku
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

finished :: Sudoku
finished =
  [ "125783649",
    "796245183",
    "483961572",
    "857412936",
    "219836457",
    "364579821",
    "541397268",
    "632158794",
    "978624315"
  ]

main :: IO ()
main = do
  if istValide finished
    then putStrLn "True"
    else putStrLn "False"

--forever (getLine >>= putStrLn)
