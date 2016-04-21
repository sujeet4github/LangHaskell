{- Tetris

   Definition and operations on Tetris pieces
-}

module Tetris.Piece where

import Tetris.Constants
import Tetris.Square

import Graphics.Gloss

import System.Random.TF.Instances
import System.Random.TF.Gen
import Data.Monoid
import Control.Arrow

-- | An 'Offset' is the amount of *squares* (in both directions) to move a
-- 'Location'
type Offset = Location

-- | Apply an offset to a location
(+.) :: Location -> Offset -> Location
(x, y) +. (dx, dy) = (x + dx, y + dy)

-- | The different Tetris shapes
data Shape = I | L | F | O | T | S | Z
  deriving (Show, Enum, Bounded)

-- Allow random generation of shapes
instance Random Shape where
  randomR = randomEnum
  random  = randomEnum (minBound, maxBound)

-- | Compute the list of square offsets for a given shape
shapeOffsets :: Shape -> [Offset]
shapeOffsets I = [(-1, 0), (0, 0), (1, 0), (2, 0)]
shapeOffsets L = [(0, -1), (0, 0), (1, 0), (2, 0)]
shapeOffsets F = [(-1, 0), (0, 0), (1, 0), (1, -1)]
shapeOffsets O = [(0, 0), (0, -1), (1, 0), (1, -1)]
shapeOffsets T = [(-1, 0), (0, 0), (0, -1), (1, 0)]
shapeOffsets S = [(-1, -1), (0, -1), (0, 0), (1, 0)]
shapeOffsets Z = [(-1, 0), (0, 0), (0, -1), (1, -1)]

-- | Get the color of a given shape
shapeColor :: Shape -> Color
shapeColor I = cyan
shapeColor L = magenta
shapeColor F = violet
shapeColor O = orange
shapeColor T = yellow
shapeColor S = chartreuse
shapeColor Z = rose

-- | An orientation specifies a rotation that can be applied to a shape
type Orientation = Offset -> Offset

-- | Rotate the orientation 90 degrees clockwise
rotateOri :: Orientation -> Orientation
rotateOri ori (dx, dy) = ori (dy, -dx)

-- | A piece represents one Tetris piece, as it's falling
data Piece = Piece { piece_shape  :: Shape
                   , piece_loc    :: Location
                   , piece_orient :: Orientation }

-- | Get the locations of the squares of a piece
pieceLocs :: Piece -> [Location]
pieceLocs (Piece { piece_shape = shape, piece_loc = loc, piece_orient = ori })
  = map ((loc +.) . ori) $ shapeOffsets shape

-- | Get the color of a piece
pieceColor :: Piece -> Color
pieceColor = shapeColor . piece_shape

-- | Internal function to move pieces, given a transformation on locations
movePiece :: (Location -> Location) -> Piece -> Piece
movePiece loc_f p@(Piece { piece_loc = loc }) = p { piece_loc = loc_f loc }

-- | Move a piece down
pieceDown :: Piece -> Piece
pieceDown = movePiece $ second (subtract 1)

-- | Move a piece left
pieceLeft :: Piece -> Piece
pieceLeft = movePiece $ first (subtract 1)

-- | Move a piece right
pieceRight :: Piece -> Piece
pieceRight = movePiece $ first (+ 1)

-- | Rotate a piece
pieceRotate :: Piece -> Piece
pieceRotate p@(Piece { piece_orient = ori }) = p { piece_orient = rotateOri ori }

-- | Render a piece into a picture
renderPiece :: Piece -> Picture
renderPiece piece = mconcat $ zipWith renderSquare (pieceLocs piece)
                                                   (repeat (Just $ pieceColor piece))

-- | Generate a new random piece, located at the appropriate spot for the "next" window
newPiece :: RandomGen g => g -> (Piece, g)
newPiece gen
  = let (shape, gen') = random gen in
    ( Piece { piece_shape = shape
            , piece_loc = nextWindowOrigin
            , piece_orient = id }
    , gen' )

-- | Set the location of a piece to be the top of the field
putPieceOverField :: Piece -> Piece
putPieceOverField p = p { piece_loc = ( fieldWidth `div` 2, fieldHeight - 1 ) }

-- | Place a piece off the screen
hidePiece :: Piece -> Piece
hidePiece p = p { piece_loc = (0, fieldHeight * 2) }
