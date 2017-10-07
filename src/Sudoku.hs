module Sudoku where

import Data.Char
--import Data.List
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game


windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

windowOffsetLeft :: Int
windowOffsetLeft = 200

windowOffsetTop :: Int
windowOffsetTop = 70

borderThickness :: Float
borderThickness = 3

tableSide::Float
tableSide = 549


data Image = Image {pic :: Picture, val :: Int}

type Images = [Image]

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
    deriving (Show)

data Window = Window
    { sudokuTable      :: Sudoku
    , someCellSelected :: Bool
    , currentCellIdx   :: (Int, Int)
    , images           :: Images
    }

getIthImage :: Picture -> Int -> Image
getIthImage pict i = Image {pic = scale 0.05 0.05 pict, val = i}

loadIms :: Int -> IO Images
loadIms i | i > 9     = do
                return []
          | otherwise = do
                theRest         <- loadIms (i+1)
                Just ithPicture <- loadJuicyJPG ("images/" ++ [intToDigit i] ++ ".jpg")
                return ((getIthImage ithPicture i) : theRest)

loadImages :: IO Images
loadImages = (loadIms 1)

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

--TODO
indexToCoord :: (Int, Int) -> (Float, Float)
indexToCoord (i,j) = (0,0)

getBordersPicture :: (Float, Float) -> Picture
getBordersPicture (x,y) = pictures [ translate x y $ color red   $ rectangleSolid 59 59
                                   , translate x y $ color white $ rectangleSolid 49 49
                                   ]

genBordersForSelectedCell :: Window -> Picture
genBordersForSelectedCell win | someCellSelected win = getBordersPicture (indexToCoord (currentCellIdx win))                                 
                              | otherwise            = blank

--TODO
generateSudokuValsImages :: Window -> Picture
generateSudokuValsImages win = pictures [blank]
                                      {-(map (map f) (rows (sudoku win))) (imageOne (images win))
                                        , translate 60 0  $ pic (imageFour (images win))
                                        , translate 122 0 $ pic (imageNine (images win))]-}

renderWindow :: Window -> Picture
renderWindow window = pictures [ generateBorders
                               , genBordersForSelectedCell window
                               , generateSudokuValsImages  window
                               ]         

clickedOnTable :: (Float, Float) -> Bool
clickedOnTable (x,y) = -tableSide/2 <= x && x <= tableSide/2
                    && -tableSide/2 <= y && y <= tableSide/2

-- TODO
coordToIndex :: (Float, Float) -> (Int, Int)
coordToIndex (x,y) | -29 <= x && x <= 29 && -29 <= y && y <= 29 = (5,5)
                   | otherwise                                  = (1,1)

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

changeListElem :: [a] -> Int -> a -> [a]
changeListElem lst i value | i == length lst = take (i-1) lst ++ [value]
                           | otherwise       = take (i-1) lst ++ [value] ++ drop i lst

changeSudokuElem :: Sudoku -> (Int, Int) -> Int -> Sudoku
changeSudokuElem s (i,j) value = Sudoku (changeListElem (rows s) i changedRow)
                               where changedRow = changeListElem oldRow j (Just value)
                                                  where oldRow = head (drop (i-1) (rows s))

renewCurrentCellVal :: Int -> Window -> Window
renewCurrentCellVal value win
    | not (someCellSelected win) = win
    | otherwise                  = win
                                   { sudokuTable      = changeSudokuElem (sudokuTable win) (currentCellIdx win) value
                                   , someCellSelected = False
                                   , currentCellIdx   = currentCellIdx win
                                   , images           = images win
                                   }


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


update :: Float -> Window -> Window 
update _ win = win

run :: IO ()
run = do
    window <- generateWindow "hard.sudoku"
    start window

start :: Window -> IO ()
start win = do
    play display bgColor fps win renderWindow handle update
    where display = InWindow "Sudoku" (windowWidth, windowHeight) (windowOffsetLeft, windowOffsetTop)
          bgColor = white
          fps     = 60

---------------------------------------------------------------------------------------

isValidSudoku :: Sudoku -> Bool
isValidSudoku s = (length (rows s)) == 9 
                  && and [length row == 9 | row <- (rows s)]
                  && and [checkRow row    | row <- (rows s)]
                  where checkRow row = and [isValidCell cell | cell <- row]
                                       where isValidCell Nothing  = True
                                             isValidCell (Just n) = n <= 9 && n >= 1
                  
parseSudoku :: String -> Sudoku
parseSudoku str | not (isValidSudoku (parse str)) = error "parseSudoku: Not a sudoku."
                | otherwise                       = parse str
                where parse = Sudoku . (map (map symToCell)) . lines
                              where symToCell '.' = Nothing
                                    symToCell ch  = Just (digitToInt ch)

readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
    file_text <- readFile path
    return (parseSudoku file_text)
