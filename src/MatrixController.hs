module MatrixController where
import Data.Time.Clock
import System.Random
import System.IO.Unsafe
import Data.List
import Debug.Trace
import Graphics.Blank

data SquareState = Empty | Set | Falling | Shadow deriving Eq
data Piece = None | Line | Square | T | OrangeL | BlueL | RedZ | GreenZ 

-- This is what the board will be storing, each square in a grid will have a state and a piece type
-- State is for actual logic in how to handle each square
-- Piece is basically just what color is it
data GridSquare = GridSquare { pieceType :: Piece, state :: SquareState}

-- TODO: Remove, this is for debugging
instance Show GridSquare where
    show square | state square == Empty = "0"
                | otherwise             = "1"

-- Matrix type declaration
type Matrix = [[GridSquare]]

-- Functions to give the size of the board
matrixWidth :: Int 
matrixWidth = 10

matrixHeight :: Int 
matrixHeight = 24

-- There will be four extra rows to handle pieces going above the visible area, so we need to mark which rows are OOB
matrixVisibleHeight :: Int 
matrixVisibleHeight = 20

-- Takes the board and puts a random piece on it using place functions
placeRandomPiece :: Matrix -> Matrix
placeRandomPiece boardMatrix
    | rand == 1 = placeLine boardMatrix
    | otherwise = placeLine boardMatrix
    -- This is bad, unsafePerformIO is bad, don't follow my example
    -- I had this to where it would return an IO Matrix where you need to splat the matrix to the screen and then continue messing with the matrix
    -- Problem is that IO and Canvas are not the same type, and there is no easy conversion between them
    -- They essentially act as the same thing, but since all the actions are using IO, the only way to do this is by some black magic with JS
    -- Or use unsafePerformIO just for this random number
    where rand = unsafePerformIO $ randomRIO (1, 6 :: Int) 

-- This places a line at the top of the board
placeLine :: Matrix -> Matrix
-- Copy the first rows that aren't visible, place line, then copy the rest in
placeLine boardMatrix = take offBoardRows boardMatrix ++ [linePieces] ++ drop (offBoardRows + 1) boardMatrix
 where offBoardRows = matrixHeight - matrixVisibleHeight
       linePieces = replicate 3 (GridSquare None Empty) ++ replicate 4 (GridSquare Line Falling) ++ replicate 3 (GridSquare None Empty)

-- This will change the board depending on what kind of tick it is, if the piece can fall, we make it fall, if not we lock it in place
tickBoard :: Matrix -> Matrix
tickBoard boardMatrix = do
    let toMove = getFallingPieces boardMatrix
    if not (null toMove)
        then
             if canFallPiece toMove boardMatrix
                then trace "MOVE" $ movePiece toMove boardMatrix
                else trace "SET" $ setPiece toMove boardMatrix
        else error "This is bad"

-- Get the location of all the falling pieces
-- Get a list of tuples with a row and its index, and then from that we get the individual elements with their index in the column
-- We then just take arrays of tuples with those indeces, so we can get the positions of all the squares that are currently falling
getFallingPieces :: Matrix -> [(Int, Int)]
getFallingPieces boardMatrix = [(i, j) | (i, rows) <- zip [0..] boardMatrix, (j, square) <- zip [0..] rows, state square == Falling]

-- Copy boardMatrix but move the falling pieces down
movePiece :: [(Int, Int)] -> Matrix -> Matrix
movePiece toMove boardMatrix = mapBoard boardMatrix moveSquare where
    moveSquare (row, col)
        -- If the square above the current one is falling, we then set the current square to that one
        | (row - 1, col) `elem` toMove                 = boardMatrix !! (row - 1) !! col
        -- If the state is currently falling, then we will set the current one to empty since the square will have fallen
        | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty
        -- Otherwise we just copy it over
        | otherwise                                    = boardMatrix !! row !! col

-- Copy board matrix but set falling pieces to set pieces
setPiece :: [(Int, Int)] -> Matrix -> Matrix
setPiece toMove boardMatrix = mapBoard boardMatrix setSquare where
    setSquare (row, col)
        | state (boardMatrix !! row !! col) == Falling = GridSquare (pieceType (boardMatrix !! row !! col)) Set
        | otherwise                                    = boardMatrix !! row !! col

-- This function allows you to remap the board given a function that changes old squares to new squares
mapBoard :: Matrix -> ((Int, Int) -> GridSquare) -> Matrix
mapBoard boardMatrix mapFunc = joinRow 0 where
    -- Loop through rows first, add each new col up
    joinRow row
        | (row + 1) < matrixHeight = joinCol 0 : joinRow (row + 1)
        | otherwise                = [joinCol 0] where
            -- Then loop through cols, apply the function to each square and turn those into a list for the new Matrix
            joinCol col
                | (col + 1) < matrixWidth = mapFunc (row, col) : joinCol (col + 1)
                | otherwise               = [mapFunc (row, col)]

-- Checks if a piece can fall by looking at the pieces below it
canFallPiece :: [(Int, Int)] -> Matrix -> Bool
canFallPiece toMove boardMatrix = trace (show boardMatrix) $ (maximum [fst i | i <- toMove]) /= (matrixHeight - 1)
    && all (\ (row, col) -> state (boardMatrix !! (row + 1) !! col) /= Set) toMove
