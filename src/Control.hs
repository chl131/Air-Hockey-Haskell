module Control where


import Types
import Model.Ball
import Model
import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Control.Monad.IO.Class (MonadIO(liftIO))

-- read the keyboard input, and respond with corresponding performance
control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tick                   -> nextS s (nextResult (ball1 s) (racket1 s) (racket2 s)) (nextResult (ball2 s) (racket1 s) (racket2 s))
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move2 up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move2 down  s)
  T.VtyEvent (V.EvKey (V.KChar 'w') _)  -> Brick.continue (move1 up  s)
  T.VtyEvent (V.EvKey (V.KChar 's') _) -> Brick.continue (move1 down s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-- if up/down/'w'/'s' key is pressed, then move the racket1/racket2
-------------------------------------------------------------------------------
move1 :: (Int -> Int) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move1 f s = s { racket1 = f (racket1 s) }

move2 :: (Int -> Int) -> PlayState -> PlayState
move2 f s = s { racket2 = f (racket2 s) }

-- check the distance rackets move based on upward or downward direction
up :: Int -> Int
up r = min (boardHeight-3) (r+1)
down :: Int -> Int
down r = max 2 (r-1)

-- change to next state
-------------------------------------------------------------------------------
nextS :: PlayState -> Result Ball -> Result Ball -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b1 b2 = case next s b1 b2 of
  Right s' -> continue =<< liftIO s'
  Left (res, sc) -> continue (s { result = res, score = sc}) 
  -- continue for Left to show who the winner is on the screen
  --Left res -> halt (s { result = res }) 