{- Tetris

   Definition and operations for the Tetris field
-}

module Tetris.Field where

import Tetris.Square
import Tetris.Piece
import Tetris.Constants

import Graphics.Gloss

import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List

-- | A field is a 2-dimensional vector of squares
type Field = V.Vector (V.Vector Square)

-- | An empty field
blankField :: Field
blankField = V.replicate fieldWidth (V.replicate fieldHeight blankSquare)

-- | Retrieve the square at a location. Returns @Nothing@ is the location is out
-- of bounds.
getSquare :: Field -> Location -> Maybe Square
getSquare field (x, y) = do
  col <- field V.!? x
  col V.!? y

-- | Sets the location of a square. Ignores invalid operations.
setSquare :: Field -> (Location, Square) -> Field
setSquare field ((x, y), sq) = fromMaybe field $ do
  col <- field V.!? x
  guard $ 0 <= y && y < fieldHeight
  let col' = col V.// [(y, sq)]
      field' = field V.// [(x, col')]
  return field'

-- | Fold the squares of a field into a monoid
foldField :: Monoid m => (Location -> Square -> m) -> Field -> m
foldField f
  = V.ifoldl' (\ mx x col ->
              V.ifoldl' (\ my y sq -> my <> f (x, y) sq) mx col) mempty
    
-- | Can this piece exist in the given location in the given field?
validPiece :: Field -> Piece -> Bool
validPiece field piece = isJust $ do   -- the Maybe monad
  squares <- mapM (getSquare field) (pieceLocs piece)
  guard $ all (not . squareFilled) squares

-- | Render the field into a Picture. Output is (fieldWidth, fieldHeight) in size.
renderField :: Field -> Picture
renderField = foldField renderSquare

-- | Add a piece permanently into the field.
addPiece :: Field -> Piece -> Field
addPiece field piece
  = let locs = pieceLocs piece
        squares = map (const $ Just (pieceColor piece)) locs
        field' = foldl' setSquare field $ zip locs squares
    in
    eliminateLines field'

-- | Remove all filled lines from the field
eliminateLines :: Field -> Field
eliminateLines field
  = let filled_lines = [ i | i <- [0..fieldHeight - 1]
                           , let locs = [ (x,i) | x <- [0..fieldWidth - 1] ]
                           , all (squareFilled . fromMaybe blankSquare) $
                             map (getSquare field) locs ]
    in
    V.map (\v -> foldr zapElement v filled_lines) field

-- | Remove a square from a column
zapElement :: Int -> V.Vector Square -> V.Vector Square
zapElement i v = let (before, after) = V.splitAt i v in
                 before V.++ V.tail after `V.snoc` blankSquare
