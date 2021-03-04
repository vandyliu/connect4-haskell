module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Char
import Play
import Connect4
import Players

-- Window
window = InWindow "Connect Four" (900, 900) (100, 100)

-- Connect 4 grid height
winheight :: Float
winheight = 700


-- Connect 4 grid width
winwidth :: Float
winwidth = 700

-- Constants
bgcolor = makeColorI 28 148 252 255
markerSize = 40
markerEmptyColor = makeColorI 225 225 225 255
markerRedColor = makeColorI 245 5 8 255
markerYellowColor = makeColorI 248 217 9 255
boardNumberY = (-370)

--boardNumbers displays the numbers under the board
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

-- winningText displays the winner's text after there is a winner
winningText :: Picture
winningText = translate 20 370 (Color white (Scale 0.5 0.5 (Text " won!")))

-- gameAsPicture draws the current board state
gameAsPicture :: Result -> IO Picture
gameAsPicture (EndOfGame val (State (_, playerTurn, board) _) start_state) = return
    (pictures ([  colAsPicture (board !! 0) 0, 
                colAsPicture (board !! 1) 1, 
                colAsPicture (board !! 2) 2, 
                colAsPicture (board !! 3) 3, 
                colAsPicture (board !! 4) 4, 
                colAsPicture (board !! 5) 5, 
                colAsPicture (board !! 6) 6,
                translate 0 400 (color playerColor (circleSolid markerSize))] ++ boardNumbers ++ [winningText]))
                where playerColor = if playerTurn == Red then markerYellowColor else markerRedColor

gameAsPicture (ContinueGame (State (_, playerTurn, board) _)) = return
    (pictures ([  colAsPicture (board !! 0) 0, 
                colAsPicture (board !! 1) 1, 
                colAsPicture (board !! 2) 2, 
                colAsPicture (board !! 3) 3, 
                colAsPicture (board !! 4) 4, 
                colAsPicture (board !! 5) 5, 
                colAsPicture (board !! 6) 6, 
                translate 0 400 (color playerColor (circleSolid markerSize))] ++ boardNumbers))
                where playerColor = if playerTurn == Yellow then markerYellowColor else markerRedColor

-- colAsPicture draws a column for the board
colAsPicture :: [TeamColour] -> Float -> Picture
colAsPicture col c = pictures [ nodeAsPicture (col !! 5) 0 c,
                                nodeAsPicture (col !! 4) 1 c,
                                nodeAsPicture (col !! 3) 2 c, 
                                nodeAsPicture (col !! 2) 3 c,
                                nodeAsPicture (col !! 1) 4 c,
                                nodeAsPicture (col !! 0) 5 c ]

-- nodeAsPicture draws a single node on the board
nodeAsPicture :: TeamColour -> Float -> Float -> Picture
nodeAsPicture node c r 
    | node == Empty = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerEmptyColor (circleSolid markerSize))
    | node == Red = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerRedColor (circleSolid markerSize))
    | node == Yellow = translate (winheight/6 * r - 350 ) (winwidth - winwidth/7 * c - 450) (color markerYellowColor (circleSolid markerSize))

-- handleKeys responds to key events.
-- For an X keypress, place marker in X column.
handleKeys :: Player -> Event -> Result -> IO Result
handleKeys cpu (EventKey (Char '1') Up _ _) result =
    handleKeysHelper cpu 1 result

handleKeys cpu (EventKey (Char '2') Up _ _) result =
    handleKeysHelper cpu 2 result

handleKeys cpu (EventKey (Char '3') Up _ _) result =
    handleKeysHelper cpu 3 result

handleKeys cpu (EventKey (Char '4') Up _ _) result =
    handleKeysHelper cpu 4 result

handleKeys cpu (EventKey (Char '5') Up _ _) result =
    handleKeysHelper cpu 5 result

handleKeys cpu (EventKey (Char '6') Up _ _) result =
    handleKeysHelper cpu 6 result

handleKeys cpu (EventKey (Char '7') Up _ _) result =
    handleKeysHelper cpu 7 result

-- Do nothing for all other events.
handleKeys cpu _ game = return game

-- handleKeysHelper is a helper for handleKeys
handleKeysHelper :: Player -> Int -> Result -> IO Result
handleKeysHelper cpu num (ContinueGame (State is actions)) = 
    if Action num `elem` actions then
        computerMove connect4 (connect4 (Action num) (State is actions)) cpu
    else
        return (ContinueGame (State is actions))

handleKeysHelper _ _ game = return game

-- Updates state of game
-- We don't actually use it though because we aren't simulating anything with respect to time
update :: Float -> Result -> IO Result
update _ res = return res

-- Main game function
main :: IO ()
main = do
    cpuAlgo <- getCPUAlgo
    startingState <- getStartingPlayer cpuAlgo
    Graphics.Gloss.Interface.IO.Game.playIO window bgcolor 30 startingState gameAsPicture (handleKeys cpuAlgo) update

-- getStartingPlayer asks the player who goes first
getStartingPlayer :: Player -> IO Result
getStartingPlayer cpu = do
    putStrLn "Who starts? 0=computer, any other key=you."
    startingPlayer <- getLine
    if startingPlayer == "0" then
        computerMove connect4 (ContinueGame connect4Start) cpu
    else do
        return (ContinueGame connect4Start)

-- getCPUAlgo asks the player what CPU Algo they want to play against
getCPUAlgo :: IO Player
getCPUAlgo = do
    putStrLn "Who do you want to play against?"
    putStrLn "0: random CPU, 1: Monte Carlo algorithm CPU, 2: Monte Carlo-Minimax hybrid CPU, any other key: simple CPU."
    line <- getLine
    if line == "0" then
        return randomPlayer
    else if line == "1" then do
        getMonteCarloPlayerOptions
    else if line == "2" then do
        getHybridPlayerOptions
    else
        return simplePlayer

-- getHybridPlayerOptions asks the player what hybrid options the CPU will have
getHybridPlayerOptions :: IO Player
getHybridPlayerOptions = do
    putStrLn "How many games do you want the hybrid Monte Carlo to try for each move? Default: 300"
    games <- getLine
    if and [isDigit c | c <- games] then do
        putStrLn ("Hybrid Monte Carlo will play "++games++" games.")
        return (hybridPlayer 19 (read games))
    else do
        putStrLn "Hybrid Monte Carlo will play 300 games."
        return (hybridPlayer 19 300)

-- getMonteCarloPlayerOptions asks the player what Monte Carlo algorithm options the CPU will have
getMonteCarloPlayerOptions :: IO Player 
getMonteCarloPlayerOptions = do
    putStrLn "How many games do you want Monte Carlo to try for each move? Default: 300"
    games <- getLine
    if and [isDigit c | c <- games] then do
        putStrLn ("Monte Carlo will play "++games++" games.")
        return (monteCarloPlayer (read games))
    else do
        putStrLn "Monte Carlo will play 300 games."
        return (monteCarloPlayer 300)
