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

-- TODO: Remove, this is for debugging
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
  newBoard <- getNewBoard context level framesSinceDrop boardMatrix gameState
  newState <- getNewState context level framesSinceDrop newBoard    gameState

  let newLevel = level -- if gameState == Drop then level else level
  let newFrames = if newState /= gameState then 0 else framesSinceDrop + 1

  send context $ do
    unless (newState == gameState) $ updateBoard context boardMatrix
    
  threadDelay (20 * 1000)
  gameLoop context newLevel newFrames newBoard newState

getNewBoard :: DeviceContext -> Int -> Int -> Matrix -> GameState -> IO Matrix
getNewBoard context level framesSinceDrop boardMatrix gameState
  | gameState == Drop = do
    clearedBoard <- clearFullRows  boardMatrix
    placeRandomPiece clearedBoard
  | gameState == Tick = return $ tickBoard boardMatrix
  | otherwise         = processInput context boardMatrix

getNewState :: DeviceContext -> Int -> Int -> Matrix -> GameState -> IO GameState
getNewState context level framesSinceDrop boardMatrix gameState = do
  bruh where bruh
              | gameState == Drop = return Playing
              | gameState == Tick = return $ if null (getFallingPieces boardMatrix) then Drop else Playing
              | framesSinceDrop >= floor (40 * startFallTime * ((1/2) ** fromIntegral level)) = return Tick
              | otherwise = return Playing

processInput :: DeviceContext -> Matrix -> IO Matrix 
processInput context boardMatrix = do
  -- Gets all events that haven't been processed
  eventList <- flush context -- I had to read the source code of blank-canvas to find this, but I'm proud that I found it
  if (not . null) eventList
    then processEvent boardMatrix eventList 0
    else return boardMatrix
      where processEvent boardMatrix eventList index
              | length eventList > (index + 1) = do
                let newBoard = controlBoard (eWhich (eventList !! index)) boardMatrix
                when (snd newBoard) $ send context $ updateBoard context (fst newBoard)
                processEvent (fst newBoard) eventList (index + 1)
              | otherwise = do
                  let newBoard = controlBoard (eWhich (eventList !! index)) boardMatrix
                  when (snd newBoard) $ send context $ updateBoard context (fst newBoard)
                  return $ fst newBoard

-- Updates the baord after a canvas redraw
updateBoard :: DeviceContext -> Matrix -> Canvas ()
updateBoard context boardMatrix = do
  clearRect (0, 0, width context, height context)
  drawBackground context

  let shadowedBoard = getShadowPieces boardMatrix

  drawPieces context shadowedBoard
  drawGrid context

-- TODO: proper lose conditionm shadows, insta-drop, rotate other way, fix rotation, extra input on drop, holding, score, queue of next pieces