module Main where

import Sudoku (Sudoku, loeseSudoku)

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

gentle :: Sudoku
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

diabolical :: Sudoku
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

unsolvable :: Sudoku
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
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
  print "Easy"
  print (loeseSudoku easy)
  print "Gentle"
  print (loeseSudoku gentle)
  print "Diabolical"
  print (loeseSudoku diabolical)
  print "Unsolvable"
  print (loeseSudoku unsolvable)
