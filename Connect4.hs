module Connect4 where

import System.IO
import Data.List
import System.Random

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Eq, Show)

data Result = EndOfGame Double [[TeamColour]] State   -- end of game: value, end board, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> IO Action

------ Connect4 Game -------

-- an action for a player is an Int, representing the column number between 1-7 to place a piece
newtype Action = Action Int
         deriving (Ord,Eq)

data TeamColour = Red
                | Black
                | Empty
type InternalState = (Int, TeamColour, [[TeamColour]])   -- (open slots remaining, current colour's turn, 2d array of BoardSpace)


-- Update the board with the given action, then check:
-- if the board contains four in a row, the current agent wins
-- if the board has no more available moves (and no agent has won), game is a tie 
-- otherwise continue the game with the new board, and the opposite colour's move
connect4 :: Game
connect4 move state
    | win newBoard = EndOfGame 1 newBoard connect4Start
    | remaining == 0 = EndOfGame 0 newBoard connect4Start
    | otherwise =
          ContinueGame (State (remaining - 1, otherColour, newBoard)
                        newAvailableActions)
            where (State (remaining, colour, board) available_actions) = state 
                  otherColour = if colour == Red then Black else Red
                  newBoard = [if move == idx then placeMarkerOntoFirstEmptySpot col colour else col | (idx, col) <- zip [Action x | x <- [1..7]] board]  -- update the board by adding the new piece to the correct column
                  newAvailableActions = [action | (action, col) <- zip [Action x | x <- [1..7]] newBoard, [] /= (filter (== Empty)col)]   -- only keep column numbers that have an empty space 

placeMarkerOntoFirstEmptySpot :: [TeamColour] -> TeamColour -> [TeamColour]
placeMarkerOntoFirstEmptySpot [] marker = []
placeMarkerOntoFirstEmptySpot (x:xs) marker = 
    if x == Empty then marker:xs else [x] ++ placeMarkerOntoFirstEmptySpot xs marker

-- win [[TeamColour]] = given a board, determines if four pieces of the same colour in a line either horizontally, vertially, or diagaonlly
win :: [[TeamColour]] -> Bool
win board = fourVertical board || fourHorizontal board || fourDiagonal board

-- Given a board, checks if a column contains four consecutive elements of the same TeamColour
fourVertical :: [[TeamColour]] -> Bool
fourVertical board = or [fourInARow col | col <- board]

-- Given a board, checks if a row contains four consecutive elements of the same TeamColour
fourHorizontal :: [[TeamColour]] -> Bool
fourHorizontal board = or [fourInARow row | row <- transpose board]

-- Given a board, checks every square if there is a diagonal with four consecutive elements of the same TeamColour
fourDiagonal :: [[TeamColour]] -> Bool
fourDiagonal board = (or [fourInARow (getDiagonalBottomLeftTopRight board col row) | (col,row) <- zip (replicate 6 1 ++ [2..7]) ([1..6] ++ replicate 6 1) ]) ||
                    (or [fourInARow (getDiagonalTopLeftBottomRight board col row) | (col,row) <- zip (replicate 6 1 ++ [2..7]) ([1..6] ++ replicate 6 6) ])

-- Given a board, and starting position (column #, row #), returns a list of TeamColour spots on the diagonal going up and to the right.
---- exBoard = [[Black, Black, Black, Red, Empty, Empty], [Red, Red, Red, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty],  [Black, Red, Red, Red, Empty, Empty], [Red, Black, Red, Empty, Empty, Empty], [Red, Black, Empty, Empty, Empty, Empty], [Red, Red, Black, Empty, Empty, Empty]]
---- getDiagonalBottomLeftTopRight exBoard 2 1 = [Red, Empty, Red, Empty, Empty, Empty]
---- getDiagonalBottomLeftTopRight exBoard 7 6 = [Empty]
getDiagonalBottomLeftTopRight :: [[TeamColour]] -> Int -> Int -> [TeamColour]
getDiagonalBottomLeftTopRight [] _ _ = []
getDiagonalBottomLeftTopRight table colNum rowNum
    | colNum > length table || colNum < 1 = []
    | rowNum > length (table !! colIndex) || rowNum < 1 = []
    | otherwise = ((table !! colIndex) !! rowIndex) : getDiagonalBottomLeftTopRight table (colNum + 1) (rowNum + 1)
        where colIndex = colNum - 1
              rowIndex = rowNum - 1

-- Given a board, and starting position (column #, row #), returns a list of TeamColour spots on the diagonal going down and to the right.
---- exBoard = [[Black, Black, Black, Red, Empty, Empty], [Red, Red, Red, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty],  [Black, Red, Red, Red, Empty, Empty], [Red, Black, Red, Empty, Empty, Empty], [Red, Black, Empty, Empty, Empty, Empty], [Red, Red, Black, Empty, Empty, Empty]]
---- getDiagonalTopLeftBottomRight exBoard 1 4 = [Red, Red, Empty Black]
---- getDiagonalTopLeftBottomRight exBoard 2 6 = [Empty, Empty, Red, Red, Black, Red]
---- getDiagonalTopLeftBottomRight exBoard 6 3 = [Empty, X]
getDiagonalTopLeftBottomRight :: [[TeamColour]] -> Int -> Int -> [TeamColour]
getDiagonalTopLeftBottomRight [] _ _ = []
getDiagonalTopLeftBottomRight table colNum rowNum
    | colNum > length table || colNum < 1 = []
    | rowNum > length (table !! colIndex) || rowNum < 1 = []
    | otherwise = ((table !! colIndex) !! rowIndex) : getDiagonalTopLeftBottomRight table (colNum + 1) (rowNum - 1)
        where colIndex = colNum - 1
              rowIndex = rowNum - 1              

-- Given a list, return true if four consecutive elements are equal
---- fourInARow [Empty,Red,Red,Red,Red,Black] = True
---- fourInARow [Red,Red,Red] = False
---- fourInARow [Red,Empty,Red,Red,Red] = False
fourInARow :: [TeamColour] -> Bool
fourInARow [] = False
fourInARow (x:xs) = (x /= Empty && ([x,x,x] == take 3 xs)) || fourInARow xs

-- Basic start state. Red will Start, board is originally empty
connect4Start :: State
connect4Start = State (41, Red, [[Empty | _ <- [1..6]] | _ <- [1..7]]) [Action n | n <- [1..7]]

connect4LastPlayDraw :: State
-- connect4LastPlayDraw goes straight to a state of the game where there will certainly be a draw
connect4LastPlayDraw = State (1, Red, [[Black, Black, Black, Red, Empty, Empty], [Red, Red, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red], [Black, Red, Black, Black, Black, Red], [Red, Black, Red, Red, Red, Black], [Red, Red, Black, Red, Black, Black], [Red, Red, Black, Red, Black, Red]]) [Action n | n <- [1]]

connect4LastPlayWin :: State
-- connect4LastPlayWin goes straight to a state of the game where there will certainly be a winner
connect4LastPlayWin = State (1, Red, [[Black, Red, Red, Black, Red, Black], [Red, Red, Black, Red, Black, Red], [Red, Red, Black, Black, Black, Red], [Red, Black, Red, Black, Black, Black], [Black, Red, Red, Black, Empty, Empty], [Black, Red, Black, Red, Black, Red], [Black, Red, Red, Black, Red, Black]]) [Action n | n <- [5]]

-- Print the board to the output, where "X" represents Red, "O" represents Black, and "-" represents an empty space
printBoard :: [[TeamColour]] -> IO ()
printBoard board = 
    do 
        putStrLn "============="
        putStrLn (printRow 6 board)
        putStrLn (printRow 5 board)
        putStrLn (printRow 4 board)
        putStrLn (printRow 3 board)
        putStrLn (printRow 2 board)
        putStrLn (printRow 1 board)
        putStrLn "1 2 3 4 5 6 7"
        putStrLn "============="

-- Given a row number and board, prints a row from the board
printRow :: Int -> [[TeamColour]] -> [Char]
printRow n [] = []
printRow n (col:restCol) = show (col !! (n-1)) ++ " " ++ printRow n restCol

instance Eq TeamColour where
   c1 == c2 = show c1 == show c2
instance Show TeamColour where
   show Red = "X"
   show Black = "O"
   show Empty = "-"
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]



------- Players -------

simplePlayer :: Player
-- simplePlayer has an ordering of the moves, and chooses the first one available
-- The order is it will choose the middle one first, then whatever is closest to the middle next
simplePlayer (State _ avail) = do
     return (head [Action e | e <- [4, 3, 5, 2, 6, 1, 7],  Action e `elem` avail])

randomPlayer :: Player
-- randomPlayer randomly selects one action from the available moves
randomPlayer (State _ avail) = pickRandomAction [Action e | e <- [1..7], Action e `elem` avail]

pickRandomAction :: [Action] -> IO Action
pickRandomAction availActions = do
    pickIndex <- randomRIO (0, length availActions - 1)
    return (availActions !! pickIndex)

-- monteCarloPlayer plays out each move X times by selecting random moves until completion, and selects the best outcome. (Currently only checks if game end in win or loss)
monteCarloPlayer ::  Integer -> Player
monteCarloPlayer numGames state = do
    actionValuePair <- mc connect4 state numGames
    return (fst actionValuePair)

-- For each action, play out X games and return the pair with the highest expected value (most wins)
mc:: Game -> State -> Integer -> IO (Action, Double)
mc game st numGames = do
    actionValuePairs <- getIOValues [ mcPlayOutXGames game st action numGames | action <- avail]
    putStrLn (show actionValuePairs)
    return (argmax actionValuePairs)
    where State _ avail = st

-- Play out X games and return a pair with the action and sum of the results
mcPlayOutXGames :: Game -> State -> Action -> Integer -> IO (Action, Double)
mcPlayOutXGames game st action numGames = do
    runs <- getIOValues ([ mcActionResult game (game action st) | run <- [1..numGames]])
    return (action, sum runs)

-- Return the value of the resulting game. If game is not over, make a random move until the game is over and return the value.
mcActionResult:: Game -> Result -> IO Double
mcActionResult _  (EndOfGame val _ _) = return val
mcActionResult game (ContinueGame state) = do
    action <- pickRandomAction available_actions
    nextResult <- (mcActionResult game (game action state))
    return  (- nextResult)
    where (State (remaining, colour, board) available_actions) = state 
 
getIOValues :: [IO a] -> IO [a]
getIOValues [] = pure []
getIOValues (h:t) = do
    x <- h
    xs <- sequence t
    return (x:xs)

argmax :: Ord v => [(e,v)] -> (e,v)
argmax [(e,v)] = (e, v)
argmax (h:t) 
   | vh > vt = h
   | otherwise = (bt,vt)
   where
      (bt,vt) = argmax t
      vh = snd h