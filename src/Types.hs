module Types where


import Graphics.Gloss.Interface.Pure.Game


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