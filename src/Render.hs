module Render where


import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants


renderWindow :: Window -> Picture
renderWindow window = pictures [ generateTableBorders
                               , genBordersForSelectedCell window
                               , genBordersForClues        window
                               , generateSudokuValsImages  window
                               , genHintsPicture           window
                               ]

-------------------------------------------
--------THE SUDOKU TABLE BORDERS-----------
-------------------------------------------

generateTableBorders :: Picture
generateTableBorders = pictures [ -- thick vertical borders
                                  translate 273    0 $ color black $ rectangleSolid borderThickness tableSide
                                , translate 91     0 $ color black $ rectangleSolid borderThickness tableSide
                                , translate (-91)  0 $ color black $ rectangleSolid borderThickness tableSide
                                , translate (-273) 0 $ color black $ rectangleSolid borderThickness tableSide
                                  -- thin vertical borders
                                , translate 212    0 $ color black $ rectangleSolid 1 tableSide
                                , translate 152    0 $ color black $ rectangleSolid 1 tableSide
                                , translate 30     0 $ color black $ rectangleSolid 1 tableSide
                                , translate (-30)  0 $ color black $ rectangleSolid 1 tableSide
                                , translate (-152) 0 $ color black $ rectangleSolid 1 tableSide
                                , translate (-212) 0 $ color black $ rectangleSolid 1 tableSide
                                  -- thick horizontal borders
                                , translate 0 273    $ color black $ rectangleSolid tableSide borderThickness
                                , translate 0 91     $ color black $ rectangleSolid tableSide borderThickness
                                , translate 0 (-91)  $ color black $ rectangleSolid tableSide borderThickness
                                , translate 0 (-273) $ color black $ rectangleSolid tableSide borderThickness
                                  -- thin horizontal borders
                                , translate 0 212    $ color black $ rectangleSolid tableSide 1
                                , translate 0 152    $ color black $ rectangleSolid tableSide 1
                                , translate 0 30     $ color black $ rectangleSolid tableSide 1
                                , translate 0 (-30)  $ color black $ rectangleSolid tableSide 1
                                , translate 0 (-152) $ color black $ rectangleSolid tableSide 1
                                , translate 0 (-212) $ color black $ rectangleSolid tableSide 1
                                ]


------------------------------------------
---------------LOW LEVEL------------------
------------------------------------------

indexToCoord :: Index -> Coordinates
indexToCoord idx = indexToCoord1 initPos idx startIdx
                   where startIdx = (1,1)

indexToCoord1 :: Coordinates -> Index -> Index -> Coordinates
indexToCoord1 (x,y) (i,j) (cur_i, cur_j) | cur_i /= i = indexToCoord1 (x, y-(59+(offset cur_i))) (i,j) (cur_i+1, cur_j)
                                         | cur_j /= j = indexToCoord1 (x+59+(offset cur_j), y)   (i,j) (cur_i, cur_j+1)
                                         | otherwise = (x,y)
                                         where offset idx | idx `rem` 3 /= 0 = 1
                                                          | otherwise        = 3


getBordersPicture :: Color -> Float-> Picture
getBordersPicture col thickness = pictures [ color col   $ rectangleSolid 59             59
                                           , color white $ rectangleSolid (59-thickness) (59-thickness)
                                           ]

shiftTo :: Coordinates -> Picture -> Picture
shiftTo (x,y) pic = translate x y $ pic



-------------------------------------------------------
-------------DRAW BORDERS FOR SELECTED CELL------------
-------------------------------------------------------

genBordersForSelectedCell :: Window -> Picture
genBordersForSelectedCell win | not (someCellSelected win) = blank 
                              | otherwise = shiftTo (indexToCoord (currentCellIdx win)) (getBordersPicture red 10)


---------------------------------------------------
-----DRAW HINTS AND HIGHLIGHT THE CELL-------------
---------------------------------------------------

genHintsPicture :: Window -> Picture
genHintsPicture win
    | not (hintsPressed win) = blank
    | otherwise = pictures [borders, hints] 
                  where borders = pictures [ shiftTo centerCoords (getBordersPicture blue 7), getDelimiters centerCoords ]
                                  where centerCoords = (indexToCoord (currentCellIdx win))
                        hints   = getHints (-59/3, 59/3) centerCoords (hintsForCurCell win) (images win)
                                  where centerCoords = (indexToCoord (currentCellIdx win))

getDelimiters :: Coordinates -> Picture
getDelimiters (center_x, center_y)
    = pictures [ shiftTo (-59/6, 0) verticalLine
               , shiftTo (59/6, 0)  verticalLine
               , shiftTo (0, -59/6) horizonLine
               , shiftTo (0, 59/6)  horizonLine
               ]
               where verticalLine = shiftTo (center_x, center_y) (color black $ rectangleSolid 1 50)
                     horizonLine  = shiftTo (center_x, center_y) (color black $ rectangleSolid 50 1)


getHints :: Coordinates -> Coordinates -> [Maybe Int] -> Images -> Picture
getHints (x,y) center lst imgs | null lst = blank
                               | x < (59/3)  = pictures [ shiftTo (x,y) (generateHint (head lst) center imgs)
                                                         , getHints (x+59/3,y)     center (tail lst) imgs]
                               | y >= (-59/3) = pictures [ shiftTo (x,y) (generateHint (head lst) center imgs)
                                                         , getHints (-59/3,y-59/3) center (tail lst) imgs]
                               | otherwise = blank

generateHint :: Maybe Int -> Coordinates -> Images -> Picture
generateHint val coords imgs | isNothing val = blank
                             | otherwise =  shiftTo coords (scale (1/3.5) (1/3.5) $ (imagePic image_of_n))
                                            where image_of_n = head (filter checkVal imgs)
                                                               where checkVal img = (imageVal img) == fromJust val



--------------------------------------------
----HIGHLIGHT THE INITIAL VALUES------------
-------------------------------------------- 


genBordersForClues :: Window -> Picture
genBordersForClues win = pictures (map f (clues (sudokuTable win)))
                         where f ind = shiftTo (indexToCoord ind) (getBordersPicture (greyN 0.5) 7)

----------------------------------------------------
--------PRINT ALL THE SUDOKU VALUES-----------------
----------------------------------------------------


generateSudokuValsImages :: Window -> Picture
generateSudokuValsImages win = pictures (map (\c -> generateCell c (images win)) (body (sudokuTable win)))

generateCell :: Cell -> Images -> Picture
generateCell cell imgs | isNothing (cellVal cell) = blank
                       | otherwise = shiftTo (indexToCoord (cellInd cell)) (imagePic image_of_n)
                                     where image_of_n = head (filter checkVal imgs)
                                                        where checkVal img = (imageVal img) == fromJust (cellVal cell) 