module Types where


import Graphics.Gloss.Interface.Pure.Game


type Coordinates = (Float, Float)

type Index = (Int, Int)


data Image = Image { imagePic :: Picture, imageVal :: Int }
type Images = [Image]

data Cell = Cell { cellVal :: Maybe Int, cellInd :: Index}
data Sudoku = Sudoku { body :: [Cell], clues :: [Index] }
data Window = Window
    { sudokuTable      :: Sudoku
    , someCellSelected :: Bool
    , currentCellIdx   :: Index
    , hintsForCurCell  :: [Maybe Int]
    , images           :: Images
    }