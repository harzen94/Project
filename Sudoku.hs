module Sudoku where

import Data.List (transpose)

--Typdeklarationen
type Sudoku = Matrix Wert

type Matrix param = [Reihe param]

type Reihe param = [param]

type Wert = Char

--Globale Definitionen
feldgroesse :: Int
feldgroesse = 3

werte :: [Wert]
werte = ['1' .. '9']

istLeer :: Wert -> Bool
istLeer param = param == '~'

--Hilfsfunktionen
zeilen :: Matrix param -> [Reihe param]
zeilen = id

spalten :: Matrix param -> [Reihe param]
spalten = transpose

felder :: Matrix param -> [Reihe param]
felder x = packAus (map spalten (packEin x))
  where
    packEin x1 = split (map split x1)
    split x2 = aufteilen feldgroesse x2
    packAus x3 = map concat (concat x3)

aufteilen :: Int -> [param] -> [[param]]
aufteilen by [] = []
aufteilen by xs = take by xs : aufteilen by (drop by xs)

--Validierung
istValide :: Sudoku -> Bool
istValide sudoku =
  all hatKeineDuplikate (zeilen sudoku)
    && all hatKeineDuplikate (spalten sudoku)
    && all hatKeineDuplikate (felder sudoku)

hatKeineDuplikate :: Eq param => [param] -> Bool
hatKeineDuplikate [] = True
hatKeineDuplikate (x : xs) = notElem x xs && hatKeineDuplikate xs

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

--Output
