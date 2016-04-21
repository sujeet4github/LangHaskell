{- Tetris

   Define various constants needed to run the Tetris game.
-}

module Tetris.Constants where

import Graphics.Gloss

-- | The background of play areas
fieldBackground :: Color
fieldBackground = black

-- | The background of other areas
background :: Color
background = dark $ dark blue

-- | A location within a field is an x-coord and a y-coord
type Location = (Int, Int)

-- | The dimensions of the field, in number of squares
fieldHeight, fieldWidth :: Int  -- in "squares"
fieldHeight = 20
fieldWidth  = 10

-- | The sidelength of the square "next" window, measured in squares
nextWindowSize :: Int -- in "squares"
nextWindowSize = 6

-- | The origin point of the next window; near the center
nextWindowOrigin :: Location
nextWindowOrigin = (2, 2)

-- | How many pixels per square
fieldScaleFactor :: Float
fieldScaleFactor = 20

-- | The size of the field, in pixels
fieldWidthPixels, fieldHeightPixels :: Float
fieldWidthPixels = fromIntegral fieldWidth * fieldScaleFactor
fieldHeightPixels = fromIntegral fieldHeight * fieldScaleFactor

-- | The sidelength of the "next" window, in pixels
nextWindowSizePixels :: Float
nextWindowSizePixels = fromIntegral nextWindowSize * fieldScaleFactor

-- | The total size of the window, in pixels
windowWidth, windowHeight :: Int
windowWidth = 420
windowHeight = 450

-- | The total size of the window, in pixels, stored as @Float@s
windowWidthF, windowHeightF :: Float
windowWidthF = fromIntegral windowWidth
windowHeightF = fromIntegral windowHeight

-- | The starting point of the window
windowX, windowY :: Int
windowX = 200
windowY = 200

-- | Margins used throughout the layout
border :: Float
border = 25

-- | The downward shift of the "next" window
nextWindowOffset :: Float -- downshift of nextwindow
nextWindowOffset = 50

-- | How many ticks per second
beatFrequency :: Int -- in beats per second
beatFrequency = 20

-- | How many seconds between drops when the user isn't pressing down
dropFrequencySlow :: Float  -- in seconds per drop
dropFrequencySlow = 0.6

-- | How many seconds between drops when the user is pressing down
dropFrequencyFast :: Float  -- in seconds per drop
dropFrequencyFast = 0.05
