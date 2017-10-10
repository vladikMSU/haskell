module Render where


import Data.Maybe
import Graphics.Gloss.Interface.Pure.Game

import Types
import Constants


renderWindow :: Window -> Picture
renderWindow window = pictures [ generateTableBorders
                               , genBordersForSelectedCell window
                               , genHintsPicture           window
                               , genBordersForClues        window
                               , generateSudokuValsImages  window
                               ]

genHintsPicture :: Window -> Picture
genHintsPicture win | not (hintsPressed win) = blank
                    | otherwise = pictures (map (\v -> generateHint v (images win)) (hintsForCurCell win))

generateHint :: Maybe Int -> Images -> Picture
generateHint val imgs | isNothing val = blank
                      | otherwise = generatePictureAt (300, 300) (imagePic image_of_n)
                                    where image_of_n = head (filter checkVal imgs)
                                                       where checkVal img = (imageVal img) == fromJust val 

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


genBordersForSelectedCell :: Window -> Picture
genBordersForSelectedCell win | not (someCellSelected win) = blank 
                              | otherwise = generatePictureAt (indexToCoord (currentCellIdx win)) (getBordersPicture red 10)

getBordersPicture :: Color -> Float-> Picture
getBordersPicture col thickness = pictures [ color col   $ rectangleSolid 59             59
                                           , color white $ rectangleSolid (59-thickness) (59-thickness)
                                           ]

genBordersForClues :: Window -> Picture
genBordersForClues win = pictures (map f (clues (sudokuTable win)))
                         where f ind = generatePictureAt (indexToCoord ind) (getBordersPicture (greyN 0.5) 7)

generatePictureAt :: Coordinates -> Picture -> Picture
generatePictureAt (x,y) pic = translate x y $ pic

indexToCoord :: Index -> Coordinates
indexToCoord idx = indexToCoord1 initPos idx startIdx
                   where startIdx = (1,1)

indexToCoord1 :: Coordinates -> Index -> Index -> Coordinates
indexToCoord1 (x,y) (i,j) (cur_i, cur_j) | cur_i /= i = indexToCoord1 (x, y-(59+(offset cur_i))) (i,j) (cur_i+1, cur_j)
                                         | cur_j /= j = indexToCoord1 (x+59+(offset cur_j), y)   (i,j) (cur_i, cur_j+1)
                                         | otherwise = (x,y)
                                         where offset idx | idx `rem` 3 /= 0 = 1
                                                          | otherwise        = 3


generateSudokuValsImages :: Window -> Picture
generateSudokuValsImages win = pictures (map (\c -> generateCell c (images win)) (body (sudokuTable win)))

generateCell :: Cell -> Images -> Picture
generateCell cell imgs | isNothing (cellVal cell) = blank
                       | otherwise = generatePictureAt (indexToCoord (cellInd cell)) (imagePic image_of_n)
                                     where image_of_n = head (filter checkVal imgs)
                                                        where checkVal img = (imageVal img) == fromJust (cellVal cell) 