module View where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Types
import Brick(
  AttrMap, Widget, hLimit, vBox, hBox, padTop, padAll, Padding(..) ,withBorderStyle,
  str, attrMap, withAttr, emptyWidget, AttrName, on, fg, (<=>), attrName
  )
import Model.Ball

-- draw the game board and the score board, the score board is on the top of the game board
drawUI :: PlayState -> [Widget Name]
drawUI g = [C.center $ padTop (Pad 2) (drawStats g) <=> drawBoard g]

-- draw the score board, it will also show whether a player wins a game
drawStats :: PlayState -> Widget Name
drawStats g = hLimit 20
  $ vBox [ drawScore (score g)
         , padTop (Pad 1) $ drawGameOver (result g)
         ]

-- display the score of player1 and player2
-- player1's score is on the left side and player2's socre is on the right side
drawScore :: (Int, Int) -> Widget Name
drawScore (p1_score, p2_score) = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show (p1_score, p2_score)

-- display the state that a player wins the game
drawGameOver :: Maybe Turn -> Widget Name
drawGameOver Nothing = emptyWidget
drawGameOver (Just P1) = withAttr gameOverAttr $ C.hCenter $ str "PLAYER 1 WON"
drawGameOver (Just P2) = withAttr gameOverAttr $ C.hCenter $ str "PLAYER 2 WON"

{-
display the game board, the main idea is:
1. make a board (grid) with the given height and width
2. check if a coordinate (cell) of the board is a racket or ball
3. painting each cell with its attribute
-}
drawBoard :: PlayState -> Widget Name
drawBoard g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [boardHeight-1, boardHeight-2..0]]
    cellsInRow y = [drawCoord (x, y) | x <- [0..boardWidth-1]]
    drawCoord    = drawCell . cellAt
    cellAt (a, b)
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g)}        = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) + 1.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) - 1.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) + 2.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) - 2.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g)}       = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) + 1.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) - 1.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) + 2.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) - 2.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == getIntCoord (ball1 g)                            = ViewBall1
      | Coord {x=fromIntegral a+1, y=fromIntegral b} == getIntCoord (ball1 g)                          = ViewBall1
      | ((Coord {x=fromIntegral a, y=fromIntegral b} == getIntCoord (ball2 g)) && (secondBall g))      = ViewBall2
      | ((Coord {x=fromIntegral a+1, y=fromIntegral b} == getIntCoord (ball2 g)) && (secondBall g))    = ViewBall2
      | otherwise         = Empty

drawCell :: HitPlane -> Widget Name
drawCell Racket      = withAttr racketAttr cw
drawCell ViewBall1   = withAttr ball1Attr cb
drawCell ViewBall2   = withAttr ball2Attr cb
drawCell Empty       = withAttr emptyAttr cw

-- output character of rackets and ball
cw :: Widget Name
cw = str " "

cb :: Widget Name
cb = str " "

-- AttrName of each item
gameOverAttr, racketAttr, ball1Attr, ball2Attr, emptyAttr :: AttrName
gameOverAttr = attrName "gameOver"
racketAttr   = attrName "racketAttr"
ball1Attr    = attrName "ball1Attr"
ball2Attr    = attrName "ball2Attr"
emptyAttr    = attrName "emptyAttr"

-- set the color of each item
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (racketAttr, V.blue `on` V.blue)
  , (ball1Attr, V.red `on` V.red)
  , (ball2Attr, V.green `on` V.green)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]