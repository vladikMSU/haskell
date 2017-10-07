module Sudoku where

import Data.Char
--import Data.List
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game


data Image = Image {pic :: Picture, val :: Int}

type Images = [Image]

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
    deriving (Show)
data Window = Window
    { sudokuTable      :: Sudoku
    , someCellSelected :: Bool
    , currentCell      :: (Int, Int)
    , images           :: Images
    }

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

getIthImage :: Maybe Picture -> Int -> Image
getIthImage (Just pict) i = Image {pic = scale 0.05 0.05 pict, val = i}

loadIms :: [Image] -> Int -> Images
loadIms lst 9 = lst
loadIms lst i = lst ++ (getIthImage ithPicture i)
                where ithPicture = loadJuicyJPG ("images/" ++ [intToDigit i] ++ ".jpg")

loadImages :: IO Images
loadImages = loadIms [] 0

{-
    Just imOne   <- loadJuicyJPG "images/1.jpg"
    Just imTwo   <- loadJuicyJPG "images/2.jpg"
    Just imThree <- loadJuicyJPG "images/3.jpg"
    Just imFour  <- loadJuicyJPG "images/4.jpg"
    Just imFive  <- loadJuicyJPG "images/5.jpg"
    Just imSix   <- loadJuicyJPG "images/6.jpg"
    Just imSeven <- loadJuicyJPG "images/7.jpg"
    Just imEight <- loadJuicyJPG "images/8.jpg"
    Just imNine  <- loadJuicyJPG "images/9.jpg"
    return Images
        { imageOne   = 
        , imageTwo   = Image {pic = scale 0.05 0.05 imTwo,   val = 2}
        , imageThree = Image {pic = scale 0.05 0.05 imThree, val = 3}
        , imageFour  = Image {pic = scale 0.05 0.05 imFour,  val = 4}
        , imageFive  = Image {pic = scale 0.05 0.05 imFive,  val = 5}
        , imageSix   = Image {pic = scale 0.05 0.05 imSix,   val = 6}
        , imageSeven = Image {pic = scale 0.05 0.05 imSeven, val = 7}
        , imageEight = Image {pic = scale 0.05 0.05 imEight, val = 8}
        , imageNine  = Image {pic = scale 0.05 0.05 imNine,  val = 9}
        }
-}

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


generateWindow::FilePath -> IO Window
generateWindow path = do
    sudoku <- readSudoku path
    imgs   <- loadImages
    return Window
        { sudokuTable      = sudoku
        , someCellSelected = False
        , currentCell      = (0, 0)
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

generateSudokuValsImages :: Window -> Picture
generateSudokuValsImages win = pictures [ pic (imageOne (images win))
                                        , translate 60 0  $ pic (imageFour (images win))
                                        , translate 122 0 $ pic (imageNine (images win))]

changeListElem :: [a] -> Int -> a -> [a]
changeListElem lst i value | i == length lst = take (i-1) lst ++ [value]
                           | otherwise       = take (i-1) lst ++ [value] ++ drop i lst

changeSudokuElem :: Sudoku -> Int -> Int -> Int -> Sudoku
changeSudokuElem s i j value = Sudoku (changeListElem (rows s) i changedRow)
                               where changedRow = changeListElem oldRow j (Just value)
                                                  where oldRow = head (drop (i-1) (rows s))

indexToCoord :: (Int, Int) -> (Float, Float)
indexToCoord (i,j) = (0,0)

getBordersPicture :: (Float, Float) -> Picture
getBordersPicture (x,y) = translate x y $ color red $ rectangleSolid 59 59

genBordersForSelectedCell :: Window -> Picture
genBordersForSelectedCell win | someCellSelected win = getBordersPicture (indexToCoord (currentCell win))                                 
                              | otherwise            = blank

renderWindow :: Window -> Picture
renderWindow window = pictures [ generateBorders
                               , genBordersForSelectedCell window
                               , generateSudokuValsImages  window
                               ]         

clickedOnTable :: (Float, Float) -> Bool
clickedOnTable (x,y) = -tableSide/2 <= x && x <= tableSide/2
                    && -tableSide/2 <= y && y <= tableSide/2

coordToIndex :: (Float, Float) -> (Int, Int)
coordToIndex (x,y) | -29 <= x && x <= 29 && -29 <= y && y <= 29 = (5,5)
                   | otherwise                                  = (1,1)

processCellClicked :: Window -> (Float, Float) -> Window
processCellClicked win click_coords
    | not (clickedOnTable click_coords) ||
      someCellSelected win && coordToIndex click_coords == currentCell win 
        = win { sudokuTable      = sudokuTable win
              , someCellSelected = False
              , images           = images win
              , currentCell      = currentCell win
              }
    | otherwise
        = win { sudokuTable      = sudokuTable win
              , someCellSelected = True
              , images           = images win
              , currentCell      = coordToIndex click_coords
              }

renewCurrentCellVal :: Int -> Window -> Window
renewCurrentCellVal _ win = win

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