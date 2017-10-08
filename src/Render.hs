module Render where


import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants


renderWindow :: Window -> Picture
renderWindow window = pictures [ generateBorders
                               , genBordersForSelectedCell window
                               , generateSudokuValsImages  window
                               ]


generateBorders :: Picture
generateBorders = pictures [ -- thick vertical borders
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


genBordersForSelectedCell :: Window -> Picture
genBordersForSelectedCell win | someCellSelected win = getBordersPicture (indexToCoord (currentCellIdx win))                                 
                              | otherwise            = blank

getBordersPicture :: (Float, Float) -> Picture
getBordersPicture (x,y) = pictures [ translate x y $ color red   $ rectangleSolid 59 59
                                   , translate x y $ color white $ rectangleSolid 49 49
                                   ]

indexToCoord :: (Int, Int) -> (Float, Float)
indexToCoord idx = indexToCoord1 initPos idx startIdx
                   where startIdx = (1,1)

indexToCoord1 :: (Float, Float) -> (Int, Int) -> (Int, Int) -> (Float, Float)
indexToCoord1 (x,y) (i,j) (cur_i, cur_j) | cur_i /= i = indexToCoord1 (x, y-(59+(offset cur_i))) (i,j) (cur_i+1, cur_j)
                                         | cur_j /= j = indexToCoord1 (x+59+(offset cur_j), y)   (i,j) (cur_i, cur_j+1)
                                         | otherwise = (x,y)
                                         where offset idx | idx `rem` 3 /= 0 = 1
                                                          | otherwise        = 3


generateSudokuValsImages :: Window -> Picture
generateSudokuValsImages win = generateRows initPos (rows (sudokuTable win)) (images win)

generateRows :: (Float, Float) -> [[Maybe Int]] -> Images -> Picture
generateRows (x,y) rows imgs | rows == [] = blank 
                             | otherwise  = pictures [ generateRow   (x,y)             (head rows) imgs
                                                     , generateRows  (x,y-(59+offset)) (tail rows) imgs
                                                     ]
                                            where offset | ((length rows)-1) `rem` 3 /= 0 = 1
                                                         | otherwise                      = 3 

generateRow :: (Float, Float) -> [Maybe Int] -> Images -> Picture
generateRow (x,y) row imgs | row == [] = blank 
                           | otherwise = pictures [ generateCell (x,y)           (head row) imgs
                                                  , generateRow  (x+59+offset,y) (tail row) imgs
                                                  ]
                                         where offset | ((length row)-1) `rem` 3 /= 0 = 1
                                                      | otherwise                     = 3 

generateCell :: (Float, Float) -> Maybe Int -> Images -> Picture
generateCell   _    Nothing   _   = blank
generateCell (x, y) (Just n) imgs = translate x y $ picture_of_n
                                    where picture_of_n = (pic  (head (filter checkVal imgs)))
                                                         where checkVal img = (val img) == n 