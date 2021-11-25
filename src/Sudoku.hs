module Sudoku where

import Data.Char (chr, digitToInt, ord)

type FieldRow = [Int]

type Field = [FieldRow]

makeSudokuFromRow :: FieldRow -> Field
makeSudokuFromRow x = [x, x, x]

getSudokuFieldRow :: FieldRow -> String -> String
getSudokuFieldRow (x : xs) s = do
  if null xs
    then chr (ord '0' + x) : getSudokuFieldRow xs s
    else [chr (ord '0' + x)] ++ "|" ++ getSudokuFieldRow xs s
getSudokuFieldRow [] s = s

getSudokuField :: Field -> String -> String
getSudokuField (x : xs) s = do
  if null xs
    then s ++ "\t" ++ getSudokuFieldRow x s
    else s ++ "\t" ++ getSudokuFieldRow x s ++ "\n\t-----\n" ++ getSudokuField xs s
getSudokuField [] s = s
