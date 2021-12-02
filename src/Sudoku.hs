module Sudoku where

import Data.Bits (Bits (xor))
import Data.Char (chr, digitToInt, ord)
import System.Console.ANSI

type FieldRow = (Int, Int, Int)

type Field = (FieldRow, FieldRow, FieldRow)

type SudokuRow = (Field, Field, Field)

type Sudoku = (SudokuRow, SudokuRow, SudokuRow)

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt x = -1

--Input

--Input - Vorgabe:
--  Alle Zahlen als String hintereinander weg
--  Feld 1-1, Feld 1-2, Feld 1-3, Feld 2-1 ... Feld 3-3, Feld 1-4, Feld 2-4, ... Feld 3-9, Feld 4-1, Feld 4-2 ...

--  1-1 | 1-2 | 1-3   1-4 | 1-5 | 1-6   1-7 | 1-8 | 1-9
--  2-1 | 2-2 | 2-3   2-4 | 2-5 | 2-6   2-7 | 2-8 | 2-9
--  3-1 | 3-2 | 3-3   3-4 | 3-5 | 3-6   3-7 | 3-8 | 3-9

--  4-1 | 4-2 | 4-3   4-4 | 4-5 | 4-6   4-7 | 4-8 | 4-9
--  5-1 | 5-2 | 5-3   5-4 | 5-5 | 5-6   5-7 | 5-8 | 5-9
--  6-1 | 6-2 | 6-3   6-4 | 6-5 | 6-6   6-7 | 6-8 | 6-9

--  7-1 | 7-2 | 7-3   7-4 | 7-5 | 7-6   7-7 | 7-8 | 7-9
--  8-1 | 8-2 | 8-3   8-4 | 8-5 | 8-6   8-7 | 8-8 | 8-9
--  9-1 | 9-2 | 9-3   9-4 | 9-5 | 9-6   9-7 | 9-8 | 9-9

makeField :: String -> Field
makeField s = do
  let row1 = (charToInt (head s), charToInt (s !! 1), charToInt (s !! 2))
  let row2 = (charToInt (s !! 3), charToInt (s !! 4), charToInt (s !! 5))
  let row3 = (charToInt (s !! 6), charToInt (s !! 7), charToInt (s !! 8))
  (row1, row2, row3)

makeSudokuRow :: String -> SudokuRow
makeSudokuRow x = do
  let field1 = makeField (take 9 x)
  let y = drop 9 x
  let field2 = makeField (take 9 y)
  let z = drop 9 y
  let field3 = makeField (take 9 z)
  (field1, field2, field3)

makeSudoku :: String -> Sudoku
makeSudoku x = do
  let row1 = makeSudokuRow (take 27 x)
  let y = drop 27 x
  let row2 = makeSudokuRow (take 27 y)
  let z = drop 27 y
  let row3 = makeSudokuRow (take 27 z)
  (row1, row2, row3)

--Output

getSudokuFieldRow :: FieldRow -> String -> String
getSudokuFieldRow (x, y, z) s = [chr (ord '0' + x)] ++ "|" ++ [chr (ord '0' + y)] ++ "|" ++ [chr (ord '0' + z)]

getSudokuField :: Field -> String -> String
getSudokuField (x, y, z) s = getSudokuFieldRow x s ++ "\n" ++ getSudokuFieldRow y s ++ "\n" ++ getSudokuFieldRow z s

printSudoku :: Sudoku -> IO ()
printSudoku (((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)), ((d1, e1, f1), (d2, e2, f2), (d3, e3, f3)), ((g1, h1, i1), (g2, h2, i2), (g3, h3, i3))) = do
  putStrLn (getSudokuFieldRow a1 "" ++ "\t" ++ getSudokuFieldRow a2 "" ++ "\t" ++ getSudokuFieldRow a3 "")
  putStrLn (getSudokuFieldRow b1 "" ++ "\t" ++ getSudokuFieldRow b2 "" ++ "\t" ++ getSudokuFieldRow b3 "")
  putStrLn (getSudokuFieldRow c1 "" ++ "\t" ++ getSudokuFieldRow c2 "" ++ "\t" ++ getSudokuFieldRow c3 "")
  putStrLn ""
  putStrLn (getSudokuFieldRow d1 "" ++ "\t" ++ getSudokuFieldRow d2 "" ++ "\t" ++ getSudokuFieldRow d3 "")
  putStrLn (getSudokuFieldRow e1 "" ++ "\t" ++ getSudokuFieldRow e2 "" ++ "\t" ++ getSudokuFieldRow e3 "")
  putStrLn (getSudokuFieldRow f1 "" ++ "\t" ++ getSudokuFieldRow f2 "" ++ "\t" ++ getSudokuFieldRow f3 "")
  putStrLn ""
  putStrLn (getSudokuFieldRow g1 "" ++ "\t" ++ getSudokuFieldRow g2 "" ++ "\t" ++ getSudokuFieldRow g3 "")
  putStrLn (getSudokuFieldRow h1 "" ++ "\t" ++ getSudokuFieldRow h2 "" ++ "\t" ++ getSudokuFieldRow h3 "")
  putStrLn (getSudokuFieldRow i1 "" ++ "\t" ++ getSudokuFieldRow i2 "" ++ "\t" ++ getSudokuFieldRow i3 "")
