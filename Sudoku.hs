module Sudoku where

import Data.List (transpose, (\\))

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
istLeer param = param == '.'

--Hilfsfunktionen
zeilen :: Matrix param -> [Reihe param]
zeilen param = param

spalten :: Matrix param -> [Reihe param]
spalten param = transpose param

felder :: Matrix param -> [Reihe param]
felder x = packAus (map spalten (packEin x))
  where
    packEin x1 = split (map split x1)
    split x2 = aufteilen feldgroesse x2
    packAus x3 = map concat (concat x3)

aufteilen :: Int -> [param] -> [[param]]
aufteilen by [] = []
aufteilen by xs = take by xs : aufteilen by (drop by xs)

einzelnesElement :: [a] -> Bool
einzelnesElement [_] = True
einzelnesElement _ = False

hatKeineDuplikate :: Eq param => [param] -> Bool
hatKeineDuplikate [] = True
hatKeineDuplikate (x : xs) = notElem x xs && hatKeineDuplikate xs

--Lösen des Sudokus

loeseSudoku :: Sudoku -> [Sudoku]
loeseSudoku s = sucheLoesung (ungueltigeWerteEntfernen (moeglicheWerte s))

sucheLoesung :: Matrix (MoeglicheWerte) -> [Sudoku]
sucheLoesung m
  | istBlockiert m = []
  | istFertig m = generiereLoesungen m
  | otherwise = [g | m' <- kartProdMehrereMoeglichenWerte m, g <- sucheLoesung (ungueltigeWerteEntfernen m')]

--Mögliche Werte pro Kästchen

type MoeglicheWerte = [Wert]

moeglicheWerte :: Sudoku -> Matrix (MoeglicheWerte)
--Setzt die Werte 1 - 9 in die leeren Kästchen
moeglicheWerte s = map (map auswahl) s
  where
    auswahl a =
      if istLeer a
        then werte
        else [a]

-- //

--Generiere eine Liste von möglichen Lösungen

generiereLoesungen :: Matrix (MoeglicheWerte) -> [Sudoku]
--Nimmt die Matrix mit den möglichen Belegungen der einzelnen Kästchen und erstellt jede mögliche Lösung
generiereLoesungen param = kartesischesProdukt (map kartesischesProdukt param)

kartesischesProdukt :: [[param]] -> [[param]]
kartesischesProdukt [] = [[]]
kartesischesProdukt (xs : xss) = [y : ys | y <- xs, ys <- kartesischesProdukt xss]

-- //

istFertig :: Matrix (MoeglicheWerte) -> Bool
--Sudoku fertig gelöst wenn jedes Kästchen einen möglichen Wert hat
istFertig m = all (all einzelnesElement) m

--Filter für nicht-mögliche generierte Lösungen

ungueltigeWerteEntfernen :: Matrix (MoeglicheWerte) -> Matrix (MoeglicheWerte)
--Entfernt durch die Regeln unmögliche Werte aus den möglichen Werten
ungueltigeWerteEntfernen param = entferneWerte felder (entferneWerte spalten (entferneWerte zeilen param))
  where
    entferneWerte function row = function (map reduzieren (function row))

reduzieren :: Reihe (MoeglicheWerte) -> Reihe (MoeglicheWerte)
reduzieren xss = [xs `minus` einzelneElemente | xs <- xss]
  where
    einzelneElemente = concat (filter einzelnesElement xss)

minus :: MoeglicheWerte -> MoeglicheWerte -> MoeglicheWerte
xs `minus` ys = if einzelnesElement xs then xs else xs \\ ys

enthaeltKeineMoeglichenWerte :: Matrix (MoeglicheWerte) -> Bool
enthaeltKeineMoeglichenWerte param = any (any null) param

sichereMoeglichenWerte :: Matrix (MoeglicheWerte) -> Bool
--True wenn keine Dopplungen in Zeile/Spalte/Feld vorkommen
sichereMoeglichenWerte m =
  all istEinheitlich (zeilen m)
    && all istEinheitlich (spalten m)
    && all istEinheitlich (felder m)

istEinheitlich :: Reihe (MoeglicheWerte) -> Bool
istEinheitlich r = hatKeineDuplikate (concat (filter einzelnesElement r))

istBlockiert :: Matrix (MoeglicheWerte) -> Bool
--True wenn Matrix keine mögliche Lösung ist
istBlockiert m = enthaeltKeineMoeglichenWerte m || not (sichereMoeglichenWerte m)

kartProdMehrereMoeglichenWerte :: Matrix (MoeglicheWerte) -> [Matrix (MoeglicheWerte)]
--Funtkioniert wie generiereLoesungen, jedoch nur für das erste Kästchen mit mehreren Möglichkeiten
kartProdMehrereMoeglichenWerte m =
  [reihen1 ++ [reihe1 ++ [c] : reihe2] ++ reihen2 | c <- cs]
  where
    (reihen1, reihe : reihen2) = span (all einzelnesElement) m
    (reihe1, cs : reihe2) = span einzelnesElement reihe

-- //
