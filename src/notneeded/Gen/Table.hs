module Gen.Table where

import Data.Char
import Data.List
import System.Random

type SudokuTable = [[Char]]

defaultTable :: SudokuTable
defaultTable = SudokuTable
    [ ['1' '2' '3'   '4' '5' '6'   '7' '8' '9']
    , ['4' '5' '6'   '7' '8' '9'   '1' '2' '3']
    , ['7' '8' '9'   '1' '2' '3'   '4' '5' '6']
      -----------------------------------------
    , ['2' '3' '4'   '5' '6' '7'   '8' '9' '1']
    , ['5' '6' '7'   '8' '9' '1'   '2' '3' '4']
    , ['8' '9' '1'   '2' '3' '4'   '5' '6' '7']
      -----------------------------------------
    , ['3' '4' '5'   '6' '7' '8'   '9' '1' '2']
    , ['6' '7' '8'   '9' '1' '2'   '3' '4' '5']
    , ['9' '1' '2'   '3' '4' '5'   '6' '7' '8']
    ]

transp :: SudokuTable -> SudokuTable
transp s_table | length (head s_table) == 0 = []
               | otherwise                  = (map head s_table) : transpose (map tail s_table)

swapRowsInsideArea :: SudokuTable -> SudokuTable
swapRowsInsideArea s_table = randomRIO (1, 3)


swapColsInsideArea :: SudokuTable -> SudokuTable
swapRowsInsideArea s_table = randomRIO (1, 3)