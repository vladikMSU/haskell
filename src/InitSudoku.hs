module InitSudoku where


import Data.Char

import Types


readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
    file_text <- readFile path
    return (parseSudoku file_text)

parseSudoku :: String -> Sudoku
parseSudoku str | not (isValidSudoku (parse str)) = error "parseSudoku: Not a sudoku."
                | otherwise                       = parse str
                where parse = Sudoku . (map (map symToCell)) . lines
                              where symToCell '.' = Nothing
                                    symToCell ch  = Just (digitToInt ch)

isValidSudoku :: Sudoku -> Bool
isValidSudoku s = (length (rows s)) == 9 
                  && and [length row == 9 | row <- (rows s)]
                  && and [checkRow row    | row <- (rows s)]
                  where checkRow row = and [isValidCell cell | cell <- row]
                                       where isValidCell Nothing  = True
                                             isValidCell (Just n) = n <= 9 && n >= 1

