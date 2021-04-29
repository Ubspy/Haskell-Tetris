{-# LANGUAGE OverloadedStrings #-}

module DrawGrid where
import Graphics.Blank
import Control.Monad
import MatrixController
import Debug.Trace

drawBackground :: DeviceContext -> Canvas ()
drawBackground context = do
  fillStyle "#1a1a1a"
  fillRect (0, 0, width context, height context)
  strokeStyle "rgba(153, 153, 153, 0.8)"
  lineWidth gridLineWidth
  stroke ()

drawGrid :: DeviceContext -> Canvas ()
drawGrid context = do
  drawGridXLines context
  drawGridYLines context

drawPieces :: DeviceContext -> Matrix -> Canvas ()
drawPieces context boardMatrix = do
    drawPiece (matrixHeight - matrixVisibleHeight - 1) 0
    where
        drawPiece i j
          | trace (show i ++ ", " ++ show j) $ state (boardMatrix !! i !! j)  == Empty = drawNextPiece
          | otherwise = do
            fillStyle "#aa0000"
            -- Add 1 because of the end line width, we're drawing into the grid lines but these are drawn first so it's ok
            fillRect (x, y, gridSize context + 1, gridSize context + 1)
            drawNextPiece
            -- Extra gridLineWidth is because we're drawing from after the grid line
            where x = gridXPadding context + fromIntegral j          * gridLineWidth + fromIntegral j          * gridSize context
                  y = gridYPadding context + fromIntegral iCorrected * gridLineWidth + fromIntegral iCorrected * gridSize context
                  iCorrected = i - (matrixHeight - matrixVisibleHeight) -- not -1 here because we are indexing off of something new, we want i = 4 to go to i = 0
                  drawNextPiece
                    | (i + 1) < matrixHeight && j       < matrixWidth = drawPiece (i + 1) j       -- We're checking if the cols are in bounds and if the next row will be in bounds
                    | i       < matrixHeight && (j + 1) < matrixWidth = drawPiece 0       (j + 1) -- We're checking if the rows are in bounds and if the next col will be in bounds (this is under the assumption the next row is out of bounds)
                    | otherwise                                               = return ()

-- We write drawGridXLines to not take an int, then we create a partial application function
-- this makes is to we don't need to call drawGridXLines with 0, since that would be confusing for people reading the code 
drawGridXLines :: DeviceContext -> Canvas ()
drawGridXLines context = drawGridXLine 0 where
  drawGridXLine i = do
    beginPath ()
    moveTo (x, y)
    lineTo (x, y + dy)
    stroke()
    when (i < 10) $ drawGridXLine (i + 1)
    where
        x = gridXPadding context + fromIntegral i * gridSize context + fromIntegral i * gridLineWidth
        y = gridYPadding context
        dy = 20 * gridSize context + 20 * gridLineWidth

drawGridYLines :: DeviceContext -> Canvas ()
drawGridYLines context = drawGridYLine 0 where
  drawGridYLine i = do
    beginPath ()
    moveTo (x, y)
    lineTo (x + dx, y)
    stroke()
    when (i < 20) $ drawGridYLine (i + 1)
    where
      x = gridXPadding context
      dx = 10 * gridSize context + 10 * gridLineWidth
      y = gridYPadding context + fromIntegral i * gridSize context + fromIntegral i * gridLineWidth


gridXPadding :: DeviceContext -> Double
gridXPadding context = (width context - gridSize context * 10 - gridLineWidth * 11) / 2

gridYPadding :: DeviceContext -> Double 
gridYPadding context = (height context - gridSize context * 20 - gridLineWidth * 21) / 2

-- Get the size of the grid by calculating the highest size we can have with at least 60 padding in the y axis
gridSize :: DeviceContext -> Double 
gridSize context = trySize 1 where
  trySize i
    | yLeft <= minYPadding = i - 1
    | otherwise = trySize (i + 1)
    where
      yLeft = height context - (gridLineWidth * 21 + i * 20)

minYPadding :: Double 
minYPadding = 50

gridLineWidth :: Double 
gridLineWidth =  1