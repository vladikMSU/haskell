module LoadImages where


import Data.Char
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

import Types


loadImages :: IO Images
loadImages = (loadIms 1)

loadIms :: Int -> IO Images
loadIms i | i > 9     = do
                return []
          | otherwise = do
                theRest         <- loadIms (i+1)
                Just ithPicture <- loadJuicyJPG ("images/" ++ [intToDigit i] ++ ".jpg")
                return ((getIthImage ithPicture i) : theRest)

getIthImage :: Picture -> Int -> Image
getIthImage pict i = Image {imagePic = scale 0.05 0.05 pict, imageVal = i}
