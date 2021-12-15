module Model.Score where

import Types

-- ^ Left winner, Right current Score after increment
addScore :: Score -> Turn -> Either (Turn, Score) (Score)
addScore s t = let (s1,s2) = s in case t of
      -- check if the score is larger than the max score
      -- if so, set the winner
      -- otherwise, continue the game
      P1 -> if s1 +1 >= 5 then Left (P1, (5, s2))
                        else Right (s1+1,s2)
      P2 -> if s2 +1 >= 5 then Left (P2, (s1, 5))
                        else Right (s1,s2+1)