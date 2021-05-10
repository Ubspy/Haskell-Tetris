module MatrixController where
import Data.Time.Clock
import System.Random
import Data.List
import Debug.Trace
import Graphics.Blank
import Data.Time.Clock.POSIX

data SquareState = Empty | Set | Falling | Shadow deriving Eq
data Piece = None | Line | Square | T | OrangeL | BlueL | RedZ | GreenZ 

data Rotation = Clockwise | CounterClockwise deriving Eq

-- This is what the board will be storing, each square in a grid will have a state and a piece type
-- State is for actual logic in how to handle each square
-- Piece is basically just what color is it
data GridSquare = GridSquare { pieceType :: Piece, state :: SquareState }

-- TODO: Remove, this is for debugging
instance Show GridSquare where
    show square | state square == Empty = "0"
                | otherwise             = "1"

instance Show SquareState where
    show state  | state == Set     = "Set"
                | state == Falling = "Falling"
                | otherwise        = "Bruh"

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

clearFullRows :: Matrix -> IO Matrix
clearFullRows boardMatrix = do
    return $ checkRow (matrixHeight - 1) boardMatrix
        where checkRow row currentMatrix
                | (row - 1) < 0 = currentMatrix
                | all (\ square -> state square == Set) (currentMatrix !! row) = do
                    let clearedMatrix = replicate matrixWidth (GridSquare None Empty) : (take row currentMatrix ++ drop (row + 1) currentMatrix) -- Replace the cleared row with a blank row at the top
                    checkRow row clearedMatrix -- Check this row again incase now the new row in this place needs to be cleared
                | otherwise = checkRow (row - 1) currentMatrix

-- Check losing condition
canPlaceNewPiece :: Matrix -> Bool 
-- Since the largest piece when placed is 4 wide, we want to see of all 4 of those squares are empty in the first visible row
-- If they are not empty, there are very few cases where the player could come back and win, while we could make a more advanced system where we check for any empty square, I think this is sufficient
canPlaceNewPiece boardMatrix = all (\ square -> state square /= Set) (take 4 (drop ((matrixWidth - 4) `div` 2) (boardMatrix !! (matrixHeight - matrixVisibleHeight))))

-- Takes the board and puts a random piece on it using place functions
placeRandomPiece :: Matrix -> IO Matrix
placeRandomPiece boardMatrix = do
    -- Adapted from https://stackoverflow.com/questions/17909770/get-unix-epoch-time-as-int
    -- I honestly don't know how this works, I get getPOSIX time and function composition but what is <$>? no idea
    rand <- round . (1000 *) <$> getPOSIXTime
    case rand * 10000 `mod` 7 + 1 of
        1 -> return $ placeLine    boardMatrix
        2 -> return $ placeSquare  boardMatrix
        3 -> return $ placeT       boardMatrix
        4 -> return $ placeOrangeL boardMatrix
        5 -> return $ placeBlueL   boardMatrix
        6 -> return $ placeRedZ    boardMatrix
        7 -> return $ placeGreenZ  boardMatrix

-- Use mapBoard to place a pattern at the top of the board
-- I'm really proud of this function, I know to you this is probably simple, but I really think I did a good job here
placePiece :: [(Int, Int)] -> GridSquare -> Matrix -> Matrix
placePiece piecePattern squareToPlace boardMatrix = mapBoard boardMatrix placeSquare
    where pieceWidth = maximum [snd square | square <- piecePattern] + 1 -- Add 1 for zero-indexing
          pieceHeight = maximum [fst square | square <- piecePattern] + 1
          xOffset = (matrixWidth - pieceWidth) `div` 2
          yOffset
            -- If the difference in between the highest filled piece on the board and the off grid squares is less than the piece height, we place it at that max piece place minus the piece height
            | getMaxPlacedPiece boardMatrix pieceWidth - (matrixHeight - matrixVisibleHeight) < pieceHeight = getMaxPlacedPiece boardMatrix pieceWidth - pieceHeight
            | otherwise                                                                                     = matrixHeight - matrixVisibleHeight 
          piecesToPlace = [(i + yOffset, j + xOffset) | (i, j) <- piecePattern] -- Go through the pattern and add the the offsets to put it in the right place
          placeSquare (row, col)
            | (row, col) `elem` piecesToPlace = squareToPlace
            | otherwise                       = boardMatrix !! row !! col

getMaxPlacedPiece :: Matrix -> Int -> Int
-- Get the highest row where there is a piece that is in the Set state in the middle cols
getMaxPlacedPiece boardMatrix pieceWidth =  if null yIndeces then matrixHeight - 1 else minimum yIndeces
    where yIndeces = [rowIndex | (rowIndex, row) <- zip [0..] boardMatrix, (colIndex, col) <- zip [0..] row, any (\ square -> state square == Set) row && colIndex <= maxX && colIndex >= minX]
          minX           = (matrixWidth - pieceWidth) `div` 2
          maxX           = minX + pieceWidth

placeLine :: Matrix -> Matrix
placeLine = placePiece linePattern (GridSquare Line Falling)
    where linePattern = [(0, 0), (0, 1), (0, 2), (0, 3)]

placeSquare :: Matrix -> Matrix
placeSquare = placePiece squarePattern (GridSquare Square Falling)
    where squarePattern = [(0, 0), (0, 1), (1, 0), (1, 1)]

placeT :: Matrix -> Matrix
placeT = placePiece tPattern (GridSquare T Falling)
    where tPattern = [(0, 1), (1, 0), (1, 1), (2, 1)]

placeOrangeL :: Matrix -> Matrix
placeOrangeL = placePiece lPattern (GridSquare OrangeL Falling)
    where lPattern = [(0, 0), (0, 1), (1, 1), (2, 1)]

placeBlueL :: Matrix -> Matrix
placeBlueL = placePiece lPattern (GridSquare BlueL Falling)
    where lPattern = [(0, 0), (0, 1), (1, 0), (2, 0)]

placeRedZ :: Matrix -> Matrix
placeRedZ = placePiece zPattern (GridSquare RedZ Falling)
    where zPattern = [(0, 1), (1, 0), (1, 1), (2, 0)]

placeGreenZ :: Matrix -> Matrix
placeGreenZ = placePiece zPattern (GridSquare GreenZ Falling)
    where zPattern = [(0, 0), (1, 0), (1, 1), (2, 1)]

-- This will change the board depending on what kind of tick it is, if the piece can fall, we make it fall, if not we lock it in place
tickBoard :: Matrix -> Matrix
tickBoard boardMatrix = do
    let toMove = getFallingPieces boardMatrix
    if not (null toMove)
        then
            if canFallPieces toMove boardMatrix
                then movePieceDown   toMove boardMatrix
                else setFallingPiece toMove boardMatrix
        else error "This is bad"

controlBoard :: Maybe Int -> Matrix -> (Matrix, Bool)
controlBoard keyCode boardMatrix = do
    let toMove = getFallingPieces boardMatrix
    case keyCode of
        Just 37 -> (if canMovePieceLeft  toMove boardMatrix then movePieceLeft  toMove boardMatrix else boardMatrix, True)
        Just 39 -> (if canMovePieceRight toMove boardMatrix then movePieceRight toMove boardMatrix else boardMatrix, True)
        Just 69 -> (rotatePiece toMove Clockwise boardMatrix,        True)
        Just 81 -> (rotatePiece toMove CounterClockwise boardMatrix, True)
        Just 40 -> (hardDropPieces boardMatrix, True)
        _       -> (boardMatrix, False)

-- Get the location of all the falling pieces
-- Get a list of tuples with a row and its index, and then from that we get the individual elements with their index in the column
-- We then just take arrays of tuples with those indeces, so we can get the positions of all the squares that are currently falling
getFallingPieces :: Matrix -> [(Int, Int)]
getFallingPieces boardMatrix = [(i, j) | (i, rows) <- zip [0..] boardMatrix, (j, square) <- zip [0..] rows, state square == Falling]

-- Copy boardMatrix but move the falling pieces down
movePieceDown :: [(Int, Int)] -> Matrix -> Matrix
movePieceDown toMove boardMatrix = mapBoard boardMatrix moveSquare where
    moveSquare (row, col)
        -- If the square above the current one is moving, we then set the current square to that one
        | (row - 1, col) `elem` toMove                 = boardMatrix !! (row - 1) !! col
        -- If the state is currently falling, then we will set the current one to empty since the square will have fallen
        | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty
        -- Otherwise we just copy it over
        | otherwise                                    = boardMatrix !! row !! col

movePieceLeft :: [(Int, Int)] -> Matrix -> Matrix
movePieceLeft toMove boardMatrix = mapBoard boardMatrix moveSquare where
    moveSquare (row, col)
        -- If the square to the right is moving, we set the current square to that one
        | (row, col + 1) `elem` toMove                 = boardMatrix !! row !! (col + 1)
        -- Otherwise, if it's currently falling we'll set it to empty
        | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty
        -- Lastly, if it's anything else, we just copy it over
        | otherwise                                    = boardMatrix !! row !! col

movePieceRight :: [(Int, Int)] -> Matrix -> Matrix
movePieceRight toMove boardMatrix = mapBoard boardMatrix moveSquare where
    moveSquare (row, col)
        -- If the square to the left is wanting to be moved then we set that square to the current one (so we move it)
        | (row, col - 1) `elem` toMove                 = boardMatrix !! row !! (col - 1)
        -- Next, if the square is set to falling, we will have moved it, so we set it to empty
        | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty
        -- Otherwise just copy the square over
        | otherwise                                    = boardMatrix !! row !! col

-- TODO: Rotating will continually move the piece down, what would be idea is if you stored the center for a piece so it doesn't recalculate it to be at the bottom every time
-- TODO: Rotating CW breaks
rotatePiece :: [(Int, Int)] -> Rotation -> Matrix -> Matrix
rotatePiece toMove rotationDirection boardMatrix = do
    -- Gets the new squares from the getRotated squares function
    let newSquares = getRotatedSquares toMove rotationDirection
    -- Gets the piece type from one of the old squares before any rotation
    let oldSquare  = boardMatrix !! (fst . head) toMove !! (snd . head) toMove
    case canRotatePiece newSquares boardMatrix of
        -- If we can rotate the piece, then we map the board based off of the rotation algorithm
        True -> mapBoard boardMatrix rotateSquare where
            rotateSquare (row, col)
                | (row, col) `elem` newSquares                 = oldSquare             -- If it's where the new rotation will be, we set it to the square
                | state (boardMatrix !! row !! col) == Falling = GridSquare None Empty -- If it's falling, we set it to empty, since that's an old piece we would be placing over
                | otherwise = boardMatrix !! row !! col                                -- Otherwise just copy the board
        False -> boardMatrix

-- TODO: Make this more sophisticated to prevent crashing
canRotatePiece :: [(Int, Int)] -> Matrix -> Bool 
canRotatePiece newSquares boardMatrix = all (\ (row, col) -> state (boardMatrix !! row !! col) /= Set) newSquares

-- TODO: Store center so it's not constantly shifting pieces over
getRotatedSquares :: [(Int, Int)] -> Rotation -> [(Int, Int)]
getRotatedSquares toMove rotationDirection
    | rotationDirection == Clockwise        = map (\ square -> (floor (fst center) + floor (snd center) - snd square, floor (snd center) + fst square - floor (fst center))) toMove
    -- The (-0.5) is cringe but it stops the piece from constantly shifting to the left
    | rotationDirection == CounterClockwise = map (\ square -> (floor (fst center) - floor (snd center) + snd square, floor (snd center) - fst square + floor (fst center - 0.5))) toMove
        where   
            maxYCoord = maximum [fst i | i <- toMove] + 1
            minYCoord = minimum [fst i | i <- toMove]
            maxXCoord = maximum [snd i | i <- toMove] + 1
            minXCoord = minimum [snd i | i <- toMove]
            tmp = fromIntegral (maxYCoord - minYCoord) / 2
            center = (fromIntegral (maxYCoord - minYCoord) / 2 + fromIntegral minYCoord, fromIntegral (maxXCoord - minXCoord) / 2 + fromIntegral minXCoord)

-- Copy board matrix but set falling pieces to set pieces
setFallingPiece :: [(Int, Int)] -> Matrix -> Matrix
setFallingPiece toMove boardMatrix = mapBoard boardMatrix setSquare where
    setSquare (row, col)
        | state (boardMatrix !! row !! col) == Falling = GridSquare (pieceType (boardMatrix !! row !! col)) Set
        | otherwise                                    = boardMatrix !! row !! col

-- This function allows you to remap the board given a function that changes old squares to new squares
-- This is another function I'm really proud of, considering I use it all over the place I think I did a good job
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
canFallPieces :: [(Int, Int)] -> Matrix -> Bool
canFallPieces toMove boardMatrix = (maximum [fst i | i <- toMove]) /= (matrixHeight - 1) -- First check to see if it's at the bottom
    && all (\ (row, col) -> state (boardMatrix !! (row + 1) !! col) /= Set) toMove -- Then see if it collides with another piece

canMovePieceLeft :: [(Int, Int)] -> Matrix -> Bool
canMovePieceLeft toMove boardMatrix = (minimum [snd i | i <- toMove]) /= 0 -- Check to make sure we're not going OOB
    -- Check if all pieces to the left are free
    && all (\ (row, col) -> state (boardMatrix !! row !! (col - 1)) /= Set) toMove

canMovePieceRight :: [(Int, Int)] -> Matrix -> Bool
canMovePieceRight toMove boardMatrix = (maximum [snd i | i <- toMove]) /= 9 -- Check to make sure we're not going OOB
    -- Check if all pieces to the right are free
    && all (\ (row, col) -> state (boardMatrix !! row !! (col + 1)) /= Set) toMove

getShadowPieces :: Matrix -> Matrix
getShadowPieces boardMatrix = do
    tryShadowPieces $ getFallingPieces boardMatrix
    where tryShadowPieces shadows
            -- If there's no falling pieces, just return the board matrix
            | null shadows = boardMatrix
            -- If they can fall, then move them down and check again
            | canFallPieces shadows boardMatrix = tryShadowPieces [(fst shadow + 1, snd shadow) | shadow <- shadows]
            -- If they can't fall, map the board and set shadow pieces
            | otherwise = mapBoard boardMatrix placeShadows
                where placeShadows (row, col)
                        | (row, col) `elem` getFallingPieces boardMatrix = oldPiece
                        | (row, col) `elem` shadows                      = GridSquare (pieceType oldPiece) Shadow
                        | otherwise                                      = boardMatrix !! row !! col
                            -- Get the old piece so we know what to set for a shadow
                            where oldPiece = boardMatrix !! fst (head $ getFallingPieces boardMatrix) !! snd (head $ getFallingPieces boardMatrix)

hardDropPieces :: Matrix -> Matrix
hardDropPieces boardMatrix = do
    getFullyDroppedPieces $ getFallingPieces boardMatrix
        where getFullyDroppedPieces fallingPieces
                | null fallingPieces = boardMatrix
                -- If we can drop the pieces, drop them
                | canFallPieces fallingPieces boardMatrix = getFullyDroppedPieces [(fst fallingPiece + 1, snd fallingPiece) | fallingPiece <- fallingPieces]
                -- If we can't drop them, move them all the way to the bottom
                | otherwise = mapBoard boardMatrix hardDrop
                    where hardDrop (row, col)
                            | (row, col) `elem` fallingPieces                = GridSquare (pieceType oldPiece) Set
                            | (row, col) `elem` getFallingPieces boardMatrix = GridSquare None Empty
                            | otherwise                                      = boardMatrix !! row !! col  
                                where oldPiece = boardMatrix !! fst (head $ getFallingPieces boardMatrix) !! snd (head $ getFallingPieces boardMatrix)