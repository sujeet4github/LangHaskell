{- Tetris -}

module Tetris where

import Tetris.Constants
import Tetris.Field
import Tetris.Piece

import Graphics.Gloss.Interface.Pure.Game

import System.Random.TF.Gen
import System.Random.TF.Init
import Data.Monoid

data State = NotStarted | Playing | Ended

data World = World { w_field      :: Field
                   , w_piece      :: Piece  -- ^ The active, falling piece
                   , w_next_piece :: Piece  -- ^ The next piece
                   , w_state      :: State
                   , w_drop_time  :: Float  -- ^ amount of time since last drop
                   , w_dropping   :: Bool   -- ^ Is the user holding "down"?
                   , w_gen        :: TFGen }

-- | Create a suitable initial state
initialWorld :: IO World
initialWorld = do
  gen <- newTFGen
  let (first_piece, gen')  = newPiece gen
  return $ World { w_field      = blankField
                 , w_piece      = hidePiece first_piece
                 , w_next_piece = first_piece
                 , w_state      = NotStarted
                 , w_drop_time  = 0
                 , w_dropping   = False
                 , w_gen        = gen' }

-- | Step the world forward in time
step :: Float -> World -> World
step elapsed w@(World { w_field = field
                , w_piece       = piece
                , w_next_piece  = next_piece
                , w_drop_time   = drop_time
                , w_state       = Playing      -- NB: only step when Playing
                , w_dropping    = dropping
                , w_gen         = gen })
  | drop_time < allowed_drop_time              -- not ready to drop
  = w { w_drop_time = drop_time + elapsed }
  
  | validPiece field piece_moved_down          -- piece can move down
  = w { w_piece     = piece_moved_down
      , w_drop_time = 0 }

  | otherwise                                  -- piece settles
  = let field' = addPiece field piece
        piece' = putPieceOverField next_piece
        (next_piece', gen') = newPiece gen
    in
    w { w_field      = field'
      , w_piece      = piece'
      , w_next_piece = next_piece'
      , w_state      = if validPiece field' piece' then Playing else Ended
      , w_drop_time  = 0
      , w_gen        = gen' }

  where
    piece_moved_down = pieceDown piece
    allowed_drop_time
      | dropping  = dropFrequencyFast
      | otherwise = dropFrequencySlow
                    
step _ w = w  -- when not playing, don't step

-- | React to a user event
react :: Event -> World -> World
react ev w@(World { w_field = field
                  , w_piece = piece
                  , w_state = Playing })
  -- handle keypresses
  = case ev of
      EventKey (SpecialKey KeyLeft) Down _ _ ->
        w { w_piece = tryMovePiece pieceLeft field piece }
      EventKey (SpecialKey KeyRight) Down _ _ ->
        w { w_piece = tryMovePiece pieceRight field piece }
      EventKey (SpecialKey KeyUp) Down _ _ ->
        w { w_piece = tryMovePiece pieceRotate field piece }
      EventKey (SpecialKey KeyDown) Down _ _ ->
        w { w_dropping = True }
      EventKey (SpecialKey KeyDown) Up _ _ ->
        w { w_dropping = False }
      _ -> w

-- handle spacebar when game is stopped
react (EventKey (SpecialKey KeySpace) Down _ _)
      w@(World { w_gen = gen, w_next_piece = next_piece })
  = let (next_piece', gen') = newPiece gen in
    w { w_field      = blankField
      , w_piece      = putPieceOverField next_piece
      , w_next_piece = next_piece'
      , w_state      = Playing
      , w_drop_time  = 0
      , w_dropping   = False
      , w_gen        = gen' }

-- otherwise, ignore:
react _ w = w

-- | Given a piece transformer, try to move the piece. If the move is
-- impossible, do nothing.
tryMovePiece :: (Piece -> Piece) -> Field -> Piece -> Piece
tryMovePiece mover field piece
  | validPiece field moved_piece
  = moved_piece

  | otherwise
  = piece

  where
    moved_piece = mover piece

-- | Render the world into a picture.
render :: World -> Picture
render (World { w_field      = field
              , w_piece      = piece
              , w_next_piece = next_piece
              , w_state      = state })
    -- by default, (0, 0) is the center; I want it at the bottom left
  = translate (-windowWidthF / 2) (-windowHeightF / 2) $

    -- draw the field & piece in their spots
    (translate border border $
     scale fieldScaleFactor fieldScaleFactor $
     renderField field <> renderPiece piece) <>

    -- draw the "next" window
    (translate (border + fieldWidthPixels + border + border)
               (border + fieldHeightPixels - nextWindowOffset
                       - nextWindowSizePixels) $
     renderNextWindow next_piece) <>

    -- draw any instruction text
    (color magenta $
     translate (border + fieldWidthPixels + border) border $
     scale 0.2 0.2 $
     renderState state)

-- | Render instruction text, based on the current state
renderState :: State -> Picture
renderState NotStarted = (translate 0 120 $ text "Spacebar to") <>
                         text "start"
renderState Playing    = blank
renderState Ended      = (translate 0 120 $ text "Spacebar to") <>
                         text "play again"

-- | Draw the "next" window
renderNextWindow :: Piece -> Picture
renderNextWindow piece
  = color fieldBackground (polygon next_window_path) <>
    (scale fieldScaleFactor fieldScaleFactor $
     renderPiece piece)
  where
    next_window_path = [(0, 0), (nextWindowSizePixels, 0),
                        (nextWindowSizePixels, nextWindowSizePixels),
                        (0, nextWindowSizePixels)]

main :: IO ()
main = do
  world <- initialWorld
  play (InWindow "Tetris" (windowWidth, windowHeight) (windowX, windowY))
    background beatFrequency world render react step
