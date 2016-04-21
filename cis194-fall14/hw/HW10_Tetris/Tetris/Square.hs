{- Tetris

   Defines squares
-}

module Tetris.Square where

import Tetris.Constants

import Graphics.Gloss

import Data.Maybe
import Data.Monoid

-- | A square is either empty (Nothing) or is filled in with a color
type Square = Maybe Color

-- | Get the color to render for a square
squareColor :: Square -> Color
squareColor = fromMaybe fieldBackground

-- | Is this square filled?
squareFilled :: Square -> Bool
squareFilled = isJust

blankSquare :: Square
blankSquare = Nothing

-- | Render a square at the given location. The rendering is done in units of
-- squares, *not* pixels.
renderSquare :: Location -> Square -> Picture
renderSquare (x, y) sq =
  color (squareColor sq) (polygon path) <>
  color black            (line    path)
  where
    fx = fromIntegral x
    fy = fromIntegral y
    path = [(fx, fy), (fx+1, fy), (fx+1, fy+1), (fx, fy+1)]
