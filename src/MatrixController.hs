module MatrixController where
import Data.Time.Clock
import System.Random
import System.IO.Unsafe
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