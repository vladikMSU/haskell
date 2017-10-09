module InitSudoku where


import Data.Char
import Data.Maybe

import Types


readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
    file_text  <- readFile path
    sudokuBody <- parseText file_text
    cluesIdxs  <- genCluesIdxs sudokuBody
    return Sudoku
        { body  = sudokuBody
        , clues = cluesIdxs
        }

parseText :: String -> IO [Cell]
parseText file_text | not (isValidSudoku file_text) = error "parseSudoku: Not a sudoku."
                    | otherwise = do
                        return (zipWith symToCell singleString numbers)
                            where singleString = lines file_text >>= id
                                  numbers      = [1..81]

symToCell :: Char -> Int -> Cell
symToCell sym i | sym == '.' = Cell { cellVal = Nothing,               cellInd = ind }
                | otherwise  = Cell { cellVal = Just (digitToInt sym), cellInd = ind}
                where ind | i `rem` 9 == 0  = (i `quot` 9,     9)
                          | otherwise       = (i `quot` 9 + 1, i `rem` 9)


isValidSudoku :: String -> Bool
isValidSudoku file_text = let rows = lines file_text
                          in (length rows) == 9 && (and [checkRow row | row <- rows])
                          where checkRow row = (length row) == 9 && (and [checkSym s | s <- row])
                                               where checkSym '.'  = True
                                                     checkSym ch   = 1 <= digitToInt ch && digitToInt ch <= 9

genCluesIdxs :: [Cell] -> IO [Index]
genCluesIdxs cells = do
    return (map cellInd (filter (\c -> (isJust (cellVal c))) cells))