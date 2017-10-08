module Handle where


import Data.Char
import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants


handle :: Event -> Window -> Window
handle (EventKey (MouseButton LeftButton) Down _ coordinates) window = processCellClicked window coordinates
handle (EventKey (Char '1') Down _ _) window = renewCurrentCellVal (digitToInt '1') window
handle (EventKey (Char '2') Down _ _) window = renewCurrentCellVal (digitToInt '2') window
handle (EventKey (Char '3') Down _ _) window = renewCurrentCellVal (digitToInt '3') window
handle (EventKey (Char '4') Down _ _) window = renewCurrentCellVal (digitToInt '4') window
handle (EventKey (Char '5') Down _ _) window = renewCurrentCellVal (digitToInt '5') window
handle (EventKey (Char '6') Down _ _) window = renewCurrentCellVal (digitToInt '6') window
handle (EventKey (Char '7') Down _ _) window = renewCurrentCellVal (digitToInt '7') window
handle (EventKey (Char '8') Down _ _) window = renewCurrentCellVal (digitToInt '8') window
handle (EventKey (Char '9') Down _ _) window = renewCurrentCellVal (digitToInt '9') window
handle _ window = window

processCellClicked :: Window -> (Float, Float) -> Window
processCellClicked win click_coords
    | not (clickedOnTable click_coords) ||
      someCellSelected win && coordToIndex click_coords == currentCellIdx win 
        = win { sudokuTable      = sudokuTable win
              , someCellSelected = False
              , images           = images win
              , currentCellIdx   = currentCellIdx win
              }
    | otherwise
        = win { sudokuTable      = sudokuTable win
              , someCellSelected = True
              , images           = images win
              , currentCellIdx   = coordToIndex click_coords
              }

clickedOnTable :: (Float, Float) -> Bool
clickedOnTable (x,y) = -tableSide/2 <= x && x <= tableSide/2
                    && -tableSide/2 <= y && y <= tableSide/2

coordToIndex :: (Float, Float) -> (Int, Int)
coordToIndex click_coords = coordToIndex1 initPos click_coords startIdx
                            where startIdx = (1,1)

coordToIndex1 :: (Float, Float) -> (Float, Float) -> (Int, Int) -> (Int, Int)
coordToIndex1 (cur_x,cur_y) (x,y) (i,j) | inBoxWithCenter (cur_x,cur_y) (x,y) = (i,j)
                                        | j < 9     = coordToIndex1 (cur_x+59+(offset j),  cur_y) (x,y) (i,j+1)
                                        | otherwise = coordToIndex1 (init_x, cur_y-59-(offset i)) (x,y) (i+1,1)
                                        where offset idx | idx `rem` 3 /= 0 = 1
                                                         | otherwise        = 3
                                              init_x = fst initPos

inBoxWithCenter :: (Float, Float) -> (Float, Float) -> Bool
inBoxWithCenter (center_x, center_y) (x,y) =    center_x-31 <= x && x <= center_x+31
                                             && center_y-31 <= y && y <= center_y+31


renewCurrentCellVal :: Int -> Window -> Window
renewCurrentCellVal value win
    | not (someCellSelected win) = win
    | otherwise                  = win
                                   { sudokuTable      = changeSudokuElem (sudokuTable win) (currentCellIdx win) value
                                   , someCellSelected = False
                                   , currentCellIdx   = currentCellIdx win
                                   , images           = images win
                                   }

changeSudokuElem :: Sudoku -> (Int, Int) -> Int -> Sudoku
changeSudokuElem s (i,j) value = Sudoku (changeListElem (rows s) i changedRow)
                               where changedRow = changeListElem oldRow j (Just value)
                                                  where oldRow = head (drop (i-1) (rows s))

changeListElem :: [a] -> Int -> a -> [a]
changeListElem lst i value | i == length lst = take (i-1) lst ++ [value]
                           | otherwise       = take (i-1) lst ++ [value] ++ drop i lst