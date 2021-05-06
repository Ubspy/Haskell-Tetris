{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Tetris (runTetris) where
import Graphics.Blank
import Control.Concurrent
import Control.Monad
import MatrixController
import DrawGrid
import Debug.Trace

data GameState = Menu | Placed | Playing | Tick | Drop | Lost deriving Eq

-- TODO: Remove, this is for debugging
instance Show GameState where
  show state
        | state == Menu = "Menu"
        | state == Playing = "Playing"
        | state == Placed = "Placed"
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
gameLoop :: DeviceContext -> Double -> Int -> Matrix -> GameState -> IO ()
gameLoop context level framesSinceDrop boardMatrix gameState = do
  newBoard <- getNewBoard context level framesSinceDrop boardMatrix gameState
  newState <- getNewState context level framesSinceDrop newBoard    gameState

  let newLevel = if gameState == Drop then level else level + level * 0.1
  let newFrames = if newState /= gameState then 0 else framesSinceDrop + 1

  send context $ do
    -- If we've lost, show game over, otherwise update board if the game state has changed
    if newState == Lost
      then gameOver context
      else unless (newState == gameState) $ updateBoard context boardMatrix
    
  threadDelay (20 * 1000)
  -- Continune as long as the player has not lost
  when (newState /= Lost) $ gameLoop context newLevel newFrames newBoard newState

getNewBoard :: DeviceContext -> Double -> Int -> Matrix -> GameState -> IO Matrix
getNewBoard context level framesSinceDrop boardMatrix gameState
  | gameState == Drop = do
    clearedBoard <- clearFullRows  boardMatrix
    if canPlaceNewPiece boardMatrix then placeRandomPiece clearedBoard else return clearedBoard
  | gameState == Tick = return $ tickBoard boardMatrix
  | otherwise         = processInput context boardMatrix

getNewState :: DeviceContext -> Double -> Int -> Matrix -> GameState -> IO GameState
getNewState context level framesSinceDrop boardMatrix gameState
  -- If we can't place a new piece, the player has lost
  | gameState == Drop = if canPlaceNewPiece boardMatrix then return Placed else return Lost -- We have an extra "Placed" state in between Drop and Played so the drawing works properly
  | gameState == Placed = return Playing
  | gameState == Tick = return $ if null (getFallingPieces boardMatrix) then Drop else Playing
  -- This is from the hard drop, if there's no falling pieces then we have dropped
  | null (getFallingPieces boardMatrix) = return Drop
  | framesSinceDrop >= floor (40 * startFallTime * ((1/2) ** fromIntegral (floor level))) = return Tick
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

gameOver :: DeviceContext -> Canvas ()
gameOver context = do
  -- Setting the base line of the text on the y scale to be in the middle
  textBaseline MiddleBaseline
  -- Setting the align to center, this will make sure the text is drawn in the center
  textAlign CenterAnchor

  fillStyle   "#f2f2f2"
  strokeStyle "#262626"
  lineWidth    4
  font        "bold 72pt Roboto"
  fillText("Game Over!", width context / 2, height context / 2) -- Draw game over text in the middle of the screen
  strokeText("Game Over!", width context / 2, height context / 2)
  return ()

-- TODO: proper lose conditionm shadows, insta-drop, rotate other way, fix rotation, extra input on drop, holding, score, queue of next pieces