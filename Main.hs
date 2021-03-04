module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Play
import Connect4
import Players

-- define the window
window = InWindow "Connect Four" (900, 900) (100, 100)

-- connect 4 grid height
winheight :: Float
winheight = 700


--connect 4 grid width
winwidth :: Float
winwidth = 700

--some constants
bgcolor = makeColorI 28 148 252 255
markerSize = 40
markerEmptyColor = makeColorI 225 225 225 255
markerRedColor = makeColorI 245 5 8 255
markerYellowColor = makeColorI 248 217 9 255

boardNumberY = (-370)

boardNumbers :: [Picture]
boardNumbers =
    [
         translate (-370) boardNumberY (Color white (Scale 0.5 0.5 (Text "1"))),
         translate (-260) boardNumberY (Color white (Scale 0.5 0.5 (Text "2"))),
         translate (-140) boardNumberY (Color white (Scale 0.5 0.5 (Text "3"))),
         translate (-20) boardNumberY (Color white (Scale 0.5 0.5 (Text "4"))),
         translate 100 boardNumberY (Color white (Scale 0.5 0.5 (Text "5"))),
         translate 210 boardNumberY (Color white (Scale 0.5 0.5 (Text "6"))),
         translate 325 boardNumberY (Color white (Scale 0.5 0.5 (Text "7")))
    ]

gameAsPicture :: Result -> Picture
gameAsPicture (ContinueGame (State (_, playerTurn, board) _)) = 
    pictures ([  colAsPicture (board !! 0) 0, 
                colAsPicture (board !! 1) 1, 
                colAsPicture (board !! 2) 2, 
                colAsPicture (board !! 3) 3, 
                colAsPicture (board !! 4) 4, 
                colAsPicture (board !! 5) 5, 
                colAsPicture (board !! 6) 6, 
                translate 0 400 (color playerColor (circleSolid markerSize))] ++ boardNumbers)
                where playerColor = if playerTurn == Black then markerYellowColor else markerRedColor

-- function to draw a column of board bieces
colAsPicture :: [TeamColour] -> Float -> Picture
colAsPicture col c = pictures [ nodeAsPicture (col !! 5) 0 c,
                                nodeAsPicture (col !! 4) 1 c,
                                nodeAsPicture (col !! 3) 2 c, 
                                nodeAsPicture (col !! 2) 3 c,
                                nodeAsPicture (col !! 1) 4 c,
                                nodeAsPicture (col !! 0) 5 c ]

-- function to draw a single board piece
nodeAsPicture :: TeamColour -> Float -> Float -> Picture
nodeAsPicture node c r 
    | node == Empty = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerEmptyColor (circleSolid markerSize))
    | node == Red = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerRedColor (circleSolid markerSize))
    | node == Black = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerYellowColor (circleSolid markerSize))

-- | Respond to key events.
-- For a '1' keypress, place marker in 1 column.
handleKeys :: Event -> Result -> Result
handleKeys (EventKey (Char '1') _ _ _) (ContinueGame (State is actions)) =
    if Action 1 `elem` actions then 
        computerMove connect4 (connect4 (Action 1) (State is actions)) simplePlayer
    else
        ContinueGame (State is actions)
handleKeys (EventKey (Char '2') _ _ _) (ContinueGame st) =
  connect4 (Action 2) st

-- Do nothing for all other events.
handleKeys _ game = game

main :: IO ()
main = Graphics.Gloss.playIO window bgcolor 30 (ContinueGame connect4Start) gameAsPicture handleKeys (const id)

convertToIO :: Result -> IO Result 
convertToIO res = do
    return res
