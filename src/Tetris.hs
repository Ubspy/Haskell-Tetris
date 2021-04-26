{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Tetris (runTetris) where
import Graphics.Blank
import Control.Concurrent
import MatrixController
import DrawGrid
import Debug.Trace

data GameState = Menu | Playing | Drop | Lost deriving Eq

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"

-- Runs webserver for canvas on port 3000, with key press events
-- $ is a way of changing order of functions, for example: putStrLn (show (1+1)) is the same as printStrLn $ show (1+1), just a way to avoid parenthesis
-- Lamba functions in haskell are represented by \ x -> (function), so this is calling blank canvas with two arguments
-- and a function we're writing in line with the 
runTetris = blankCanvas 3000 { events = ["keydown"] } $ \ context -> gameLoop context initialBoard Drop where
  initialBoard = replicate matrixHeight (replicate matrixWidth (GridSquare None Empty))

-- Game loop function, to take care of handling the main game loop
gameLoop :: DeviceContext -> Matrix -> GameState -> IO ()
gameLoop context boardMatrix gameState = do
  send context $ do
    stateDependant
  threadDelay 25
  gameLoop context newMatrix newState
  where stateDependant | gameState == Drop = onDrop context newMatrix
                       | otherwise         = return ()
        newMatrix | gameState == Drop = placeRandomPiece boardMatrix
                  | otherwise         = boardMatrix
        newState = Menu

onDrop :: DeviceContext -> Matrix -> Canvas ()
onDrop context boardMatrix = do
  clearRect (0, 0, width context, height context)
  drawBackground context
  trace ("Board:" ++ show boardMatrix) $ drawPieces context boardMatrix
  drawGrid context
