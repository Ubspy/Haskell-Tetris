{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Tetris (runTetris) where
import Graphics.Blank
import Control.Concurrent
import Control.Monad
import MatrixController
import DrawGrid
import Debug.Trace

data GameState = Menu | Playing | Tick | Drop | Lost deriving Eq

instance Show GameState where
  show state
        | state == Menu = "Menu"
        | state == Playing = "Playing"
        | state == Tick = "Tick"
        | state == Drop = "Drop"
        | state == Lost = "Lost"

startFallTime = 1.2

--someFunc :: IO ()
--someFunc = putStrLn "someFunc"

-- Runs webserver for canvas on port 3000, with key press events
-- $ is a way of changing order of functions, for example: putStrLn (show (1+1)) is the same as printStrLn $ show (1+1), just a way to avoid parenthesis
-- Lamba functions in haskell are represented by \ x -> (function), so this is calling blank canvas with two arguments
-- and a function we're writing in line with the 
runTetris = blankCanvas 3000 { events = ["keydown"] } $ \ context -> gameLoop context 0 0 initialBoard Drop where
  initialBoard = replicate matrixHeight (replicate matrixWidth (GridSquare None Empty))

-- Game loop function, to take care of handling the main game loop
gameLoop :: DeviceContext -> Int -> Int -> Matrix -> GameState -> IO ()
gameLoop context level framesSinceDrop boardMatrix gameState = do
  send context $ do
    unless (newState == gameState) $ updateBoard context boardMatrix
    --trace (show gameState ++ ", " ++ show framesSinceDrop) $ return ()
  threadDelay 500
  gameLoop context newLevel newFrames newBoard newState
  where newLevel  | gameState == Drop = level + 1
                  | otherwise         = level
        newFrames | newState /= gameState = 0
                  | otherwise         = framesSinceDrop + 1
        newBoard  | gameState == Drop = placeRandomPiece boardMatrix
                  | gameState == Tick = tickBoard boardMatrix
                  | otherwise         = boardMatrix
        newState  | gameState == Drop = Playing
                  | gameState == Tick = if null (getFallingPieces newBoard) then Drop else Playing
                  | framesSinceDrop >= floor (40 * startFallTime * ((1/2) ** (fromIntegral level - 1))) = Tick
                  | otherwise = Playing

updateBoard :: DeviceContext -> Matrix -> Canvas ()
updateBoard context boardMatrix = do
  clearRect (0, 0, width context, height context)
  drawBackground context
  --trace ("Board:" ++ show boardMatrix) $ drawPieces context boardMatrix
  drawPieces context boardMatrix
  drawGrid context