module Handle where


import Data.Char
import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants


handle :: Event -> Window -> Window
handle (EventKey (MouseButton LeftButton) Down _ coordinates) window = processCellClicked window coordinates
handle (EventKey (Char 'h') Down _ _) window = processGetHints window
handle (EventKey (Char 'H') Down _ _) window = processGetHints window
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



--------------------------------
------PROCESS CLICKS------------
--------------------------------

processCellClicked :: Window -> Coordinates -> Window
processCellClicked win click_coords
    | not (clickedOnTable click_coords) ||
      clickedOnClue click_coords win    ||
      someCellSelected win && coordToIndex click_coords == currentCellIdx win 
        = win { someCellSelected = False
              , hintsPressed     = False
              }
    | hintsPressed win && coordToIndex click_coords == currentCellIdx win
        = processClickOnHint click_coords win
    | otherwise
        = win { someCellSelected = True
              , currentCellIdx   = coordToIndex click_coords
              , hintsPressed     = False
              }

--------LOW-LEVEL-----------

clickedOnTable :: Coordinates -> Bool
clickedOnTable (x,y) = -tableSide/2 <= x && x <= tableSide/2
                    && -tableSide/2 <= y && y <= tableSide/2

clickedOnClue :: Coordinates -> Window -> Bool
clickedOnClue click_coords win = elem (coordToIndex click_coords) (clues (sudokuTable win))

coordToIndex :: Coordinates -> Index
coordToIndex click_coords = coordToIndex1 initPos click_coords startIdx
                            where startIdx = (1,1)

coordToIndex1 :: Coordinates -> Coordinates -> Index -> Index
coordToIndex1 (cur_x,cur_y) (x,y) (i,j) | inBoxWithCenter (cur_x,cur_y) (x,y) = (i,j)
                                        | j < 9     = coordToIndex1 (cur_x+59+(offset j),  cur_y) (x,y) (i,j+1)
                                        | otherwise = coordToIndex1 (init_x, cur_y-59-(offset i)) (x,y) (i+1,1)
                                        where offset idx | idx `rem` 3 /= 0 = 1
                                                         | otherwise        = 3
                                              init_x = fst initPos

inBoxWithCenter :: Coordinates -> Coordinates -> Bool
inBoxWithCenter (center_x, center_y) (x,y) =    center_x-31 <= x && x <= center_x+31
                                             && center_y-31 <= y && y <= center_y+31

--------HIGH-LEVEL---------------------

processClickOnHint :: Coordinates -> Window -> Window
processClickOnHint click_coords win = processClickOnHint1 click_coords 1 win

processClickOnHint1 :: Coordinates -> Int -> Window -> Window
processClickOnHint1 _ _ win = win

inSmallBoxWithCenter :: Coordinates -> Coordinates -> Bool
inSmallBoxWithCenter (center_x, center_y) (x,y) =    center_x-31 <= x && x <= center_x+31
                                                  && center_y-31 <= y && y <= center_y+31
---------------------------------------------
-------------PROCESS "H" PRESS --------------
---------------------------------------------

processGetHints :: Window -> Window
processGetHints win | not (someCellSelected win) = win
                    | hintsPressed win           = win { someCellSelected = False
                                                       , hintsPressed     = False }
                    | otherwise = win { hintsForCurCell  = generateHintsList (sudokuTable win) (currentCellIdx win)
                                      , hintsPressed     = True }


generateHintsList :: Sudoku -> Index -> [Maybe Int]
generateHintsList s (i, j) =
    map f [1 .. 9]
    where f n   | not ((elem n row) || (elem n col) || (elem n sqr)) = Just n
                | otherwise = Nothing
                where row = catMaybes (map cellVal (filter (\cell -> (fst (cellInd cell)) == i) (body s)))
                      col = catMaybes (map cellVal (filter (\cell -> (snd (cellInd cell)) == j) (body s)))
                      sqr = catMaybes (map cellVal (filter (\cell -> elem (cellInd cell) (genInds (bigInd i, bigInd j))) (body s)))
                            where   bigInd ind | (ind `rem` 3) == 0 = (ind `quot` 3)
                                               | otherwise          = (ind `quot` 3) + 1
                                    genInds (bigI, bigJ) = [ ((bigI-1) * 3 + 1, (bigJ-1) * 3 + 1)
                                                           , ((bigI-1) * 3 + 1, (bigJ-1) * 3 + 2)
                                                           , ((bigI-1) * 3 + 1, (bigJ-1) * 3 + 3)
                                                           , ((bigI-1) * 3 + 2, (bigJ-1) * 3 + 1)
                                                           , ((bigI-1) * 3 + 2, (bigJ-1) * 3 + 2)
                                                           , ((bigI-1) * 3 + 2, (bigJ-1) * 3 + 3)
                                                           , ((bigI-1) * 3 + 3, (bigJ-1) * 3 + 1)
                                                           , ((bigI-1) * 3 + 3, (bigJ-1) * 3 + 2)
                                                           , ((bigI-1) * 3 + 3, (bigJ-1) * 3 + 3)
                                                           ]


------------------------------------------------
---------PROCESS OTHER KEYBORD EVENTS-----------
------------------------------------------------
                      
renewCurrentCellVal :: Int -> Window -> Window
renewCurrentCellVal value win
    | not (someCellSelected win) = win
    | hintsPressed win           = win
    | otherwise = win { sudokuTable = changeSudokuElem (sudokuTable win) (currentCellIdx win) value
                      , someCellSelected = False }

changeSudokuElem :: Sudoku -> Index -> Int -> Sudoku
changeSudokuElem s (i,j) new_val = Sudoku { body = changedBody, clues = (clues s)}
                                   where changedBody = changeListElem (body s) ind new_cell
                                                       where ind      = (i-1)*9 + j
                                                             new_cell = Cell {cellVal = Just new_val, cellInd = (i,j)}

changeListElem :: [a] -> Int -> a -> [a]
changeListElem lst i value | i == length lst = take (i-1) lst ++ [value]
                           | otherwise       = take (i-1) lst ++ [value] ++ drop i lst