module MatrixController where
import Data.Time.Clock
import System.Random
import System.IO.Unsafe
import Data.List
import Debug.Trace
import Graphics.Blank

data SquareState = Empty | Set | Falling | Shadow deriving Eq
data Piece = None | Line | Square | T | OrangeL | BlueL | RedZ | GreenZ 
data GridSquare = GridSquare { pieceType :: Piece, state :: SquareState}

instance Show GridSquare where
    show square | state square == Empty = "0"
                | otherwise             = "1"

type Matrix = [[GridSquare]]

matrixWidth :: Int 
matrixWidth = 10

matrixHeight :: Int 
matrixHeight = 24

matrixVisibleHeight :: Int 
matrixVisibleHeight = 20

placeRandomPiece :: Matrix -> Matrix
placeRandomPiece boardMatrix
    | rand == 1 = placeLine boardMatrix
    | otherwise = placeLine boardMatrix
    where rand = unsafePerformIO $ randomRIO (1, 6 :: Int) -- Bad?

-- drawSquares :: DeviceContext -> Canvas ()
-- drawSquares context = drawSquare 0 0 where
--     drawSquare x y = return ()

placeLine :: Matrix -> Matrix
placeLine boardMatrix = trace ("line: " ++ show linePieces) $ take offBoardRows boardMatrix ++ [linePieces] ++ drop (offBoardRows + 1) boardMatrix
 where offBoardRows = matrixHeight - matrixVisibleHeight
       linePieces = replicate 3 (GridSquare None Empty) ++ replicate 4 (GridSquare Line Falling) ++ replicate 3 (GridSquare None Empty)

tickBoard :: Matrix -> Matrix
tickBoard boardMatrix = do
    let toMove = [(i, j) | (i, rows) <- zip [0..] boardMatrix, (j, square) <- zip [0..] rows, state square == Falling]
    if not (null toMove)
        then
             if canMovePiece toMove boardMatrix
                then trace "MOVE" $ movePiece toMove boardMatrix
                else trace "SET" $ setPiece toMove boardMatrix
        else boardMatrix

-- Copy boardMatrix up to row where the piece is falling from
movePiece :: [(Int, Int)] -> Matrix -> Matrix
movePiece toMove boardMatrix = mapBoard boardMatrix moveSquare where
    moveSquare (row, col)
        | (row - 1, col) `elem` toMove                 = boardMatrix !! (row - 1) !! col
        | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty
        | otherwise                                    = boardMatrix !! row !! col

setPiece :: [(Int, Int)] -> Matrix -> Matrix
setPiece toMove boardMatrix = mapBoard boardMatrix setSquare where
    setSquare (row, col)
        | state (boardMatrix !! row !! col) == Falling = GridSquare (pieceType (boardMatrix !! row !! col)) Set
        | otherwise                                    = boardMatrix !! row !! col

mapBoard :: Matrix -> ((Int, Int) -> GridSquare) -> Matrix
mapBoard boardMatrix mapFunc = joinRow 0 where
    joinRow row
        | (row + 1) < matrixHeight = joinCol 0 : joinRow (row + 1)
        | otherwise                = [joinCol 0] where
            joinCol col
                | (col + 1) < matrixWidth = mapFunc (row, col) : joinCol (col + 1)
                | otherwise               = [mapFunc (row, col)]

canMovePiece :: [(Int, Int)] -> Matrix -> Bool
canMovePiece toMove boardMatrix = trace (show boardMatrix) $ (maximum [fst i | i <- toMove]) /= (matrixHeight - 1)
    && all (\ (row, col) -> state (boardMatrix !! row !! col) /= Set) toMove
