{-# LANGUAGE RecordWildCards #-}

module Model where 

import Prelude hiding ((!!))
import Types
import qualified Model.Ball   as Ball
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

init :: IO PlayState
init = do{
   b1 <- Ball.init P1;
  return PS
  { racket1     = Player.player1                  
  , racket2     = Player.player2                     
  , ball1       = b1 
  , ball2       = Ball.freeze       
  , result      = Nothing          
  , turn        = P1                    
  , score       = (0, 0)
  , secondBall  = False            
  }
}

-- generate next random ball1 and check whether ball2 should start to move
initsc1 :: PlayState -> Turn -> Score -> IO PlayState
initsc1 s p sc@(sc1, sc2) = do
  b1 <- Ball.init p
  if ((sc1 == 3 && sc2 < 3) || (sc2 == 3 && sc1 < 3)) 
    then 
      if p == P1
        then 
          do{ b2 <- Ball.init P2; return s { ball1 = b1, ball2 = b2, score = sc, secondBall = True }}
        else 
          do{ b2 <- Ball.init P1; return s { ball1 = b1, ball2 = b2, score = sc, secondBall = True }}
    else return s { ball1 = b1, score = sc }


-- generate next random ball2
initsc2 :: PlayState -> Turn -> Score -> IO PlayState
initsc2 s p sc = do{
   b2 <- Ball.init p;
  return s { ball2 = b2, score = sc }
}

-- determine two balls are staying in the current state, hitting the wall, or someone is earning the point
next :: PlayState -> Ball.Result Ball.Ball -> Ball.Result Ball.Ball -> Either ((Maybe Turn, Score)) (IO PlayState)
next s (Cont b1') (Cont b2') = Right (return (s { ball1 = b1', ball2 = b2'} ))
next s (Hit pl)   (Cont b2') = Right (return (s { ball1 = Ball.movement (Ball.reflect (ball1 s) pl), ball2 = b2' }))
next s (Cont b1') (Hit pl)   = Right (return (s { ball1 = b1', ball2 = Ball.movement (Ball.reflect (ball2 s) pl)}))
next s (Hit pl1)  (Hit pl2)  = Right (return (s { ball1 = Ball.movement (Ball.reflect (ball1 s) pl1), ball2 = Ball.movement (Ball.reflect (ball2 s) pl2)}))
next s (Score p)  _          = case (Score.addScore (score s) p) of
                                Left (winner, sc) -> Left ((Just winner), sc)
                                Right sc -> Right (initsc1 s p sc)
next s  _         (Score p)  = case (Score.addScore (score s) p) of
                                Left (winner, sc) -> Left ((Just winner), sc)
                                Right sc -> Right (initsc2 s p sc)
