module Sudoku where


import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants
import InitSudoku
import LoadImages
import Render
import Handle


run :: IO ()
run = do
    window <- generateWindow "hard.sudoku"
    start window


generateWindow::FilePath -> IO Window
generateWindow path = do
    sudoku <- readSudoku path
    imgs   <- loadImages
    return Window
        { sudokuTable      = sudoku
        , someCellSelected = False
        , currentCellIdx   = (0, 0)
        , images           = imgs
        }


start :: Window -> IO ()
start win = do
    play display bgColor fps win renderWindow handle update
    where display = InWindow "Sudoku" (windowWidth, windowHeight) (windowOffsetLeft, windowOffsetTop)
          bgColor = white
          fps     = 60


update :: Float -> Window -> Window 
update _ win = win
