module Connect4 where

import System.IO
import Data.List

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Eq, Show)

data Result = EndOfGame Double [[TeamColour]] State   -- end of game: value, end board, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

------ Connect4 Game -------
newtype Action = Action Int                 -- a move for a player is just an Int
         deriving (Ord,Eq)

data TeamColour = Red
                | Black
type InternalState = (Int, TeamColour, [[TeamColour]])   -- (open slots remaining, current colour's turn, 2d array of BoardSpace)


connect4 :: Game
connect4 move state
    | win (remaining, colour, newBoard) = EndOfGame 1 newBoard connect4Start   -- agent wins
    | remaining == 0 = EndOfGame 0 newBoard connect4Start   -- no more moves, tie
    | otherwise =
          ContinueGame (State (remaining - 1, otherColour, newBoard)
                        newAvailableActions)
            where (State (remaining, colour, board) available_actions) = state 
                  otherColour = if colour == Red then Black else Red
                  newBoard = [if move == idx then col ++ [colour] else col | (idx, col) <- zip [Action x | x <- [1..7]] board]
                  newAvailableActions = [action | (action, col) <- zip [Action x | x <- [1..7]] newBoard, length col < 6]

-- win n internalState = the agent wins if it selects a column that will leave four pieces of their colour in a line either horizontally, vertially, or diagaonlly
win :: InternalState -> Bool
win int_state = fourVertical int_state || fourHorizontal int_state || fourDiagonal int_state

fourVertical :: InternalState -> Bool
fourVertical (remaining, colour, board) = or [fourInARow col | col <- board]

fourHorizontal :: InternalState -> Bool
fourHorizontal (remaining, colour, board) = or [fourInARow row | row <- transpose board]

fourDiagonal :: InternalState -> Bool
fourDiagonal (remaining, colour, board) = or [fourInARow (getDiagonalBottomLeftTopRight board col row) || fourInARow (getDiagonalTopLeftBottomRight board col row) | col <- [1..7], row <- [1..6]]  -- Could be more efficient, checks diag of every square

getDiagonalBottomLeftTopRight :: [[TeamColour]] -> Int -> Int -> [TeamColour] -- state -> col # -> row # -> diagonal starting from bottom left to top right given coordinates
getDiagonalBottomLeftTopRight [] _ _ = []
getDiagonalBottomLeftTopRight table colNum rowNum
    | colNum > length table || colNum < 1 = []
    | rowNum > length (table !! colIndex) || rowNum < 1 = []
    | otherwise = ((table !! colIndex) !! rowIndex) : getDiagonalBottomLeftTopRight table (colNum + 1) (rowNum + 1)
        where colIndex = colNum - 1
              rowIndex = rowNum - 1

getDiagonalTopLeftBottomRight :: [[TeamColour]] -> Int -> Int -> [TeamColour] -- state -> col # -> row # -> diagonal starting from bottom left to top right given coordinates
getDiagonalTopLeftBottomRight [] _ _ = []
getDiagonalTopLeftBottomRight table colNum rowNum
    | colNum > length table || colNum < 1 = []
    | rowNum > length (table !! colIndex) || rowNum < 1 = []
    | otherwise = ((table !! colIndex) !! rowIndex) : getDiagonalTopLeftBottomRight table (colNum + 1) (rowNum - 1)
        where colIndex = colNum - 1
              rowIndex = rowNum - 1              

fourInARow :: [TeamColour] -> Bool
fourInARow [] = False
fourInARow (x:xs) = ([x,x,x] == take 3 xs) || fourInARow xs

connect4Start :: State
connect4Start = State (41, Red, [[],[],[],[],[],[],[]]) [Action n | n <- [1..7]] -- Red will Start, board is originally empty

connect4LastPlayDraw :: State
-- connect4LastPlayDraw goes straight to a state of the game where there will certainly be a draw
connect4LastPlayDraw = State (1, Red, [[Black, Black, Black, Red], [Red, Red, Red, Black, Red, Black], [Black, Red, Black, Black, Black, Red], [Black, Red, Black, Black, Black, Red], [Red, Black, Red, Red, Red, Black], [Red, Red, Black, Red, Black, Black], [Red, Red, Black, Red, Black, Red]]) [Action n | n <- [1]]

connect4LastPlayWin :: State
-- connect4LastPlayWin goes straight to a state of the game where there will certainly be a winner
connect4LastPlayWin = State (1, Red, [[Black, Red, Red, Black, Red, Black], [Red, Red, Black, Red, Black, Red], [Red, Red, Black, Black, Black, Red], [Red, Black, Red, Black, Black, Black], [Black, Red, Red, Black], [Black, Red, Black, Red, Black, Red], [Black, Red, Red, Black, Red, Black]]) [Action n | n <- [5]]

printBoard :: [[TeamColour]] -> IO ()
printBoard board = 
    do 
        putStrLn "============="
        putStrLn (getRow 6 board)
        putStrLn (getRow 5 board)
        putStrLn (getRow 4 board)
        putStrLn (getRow 3 board)
        putStrLn (getRow 2 board)
        putStrLn (getRow 1 board)
        putStrLn "1 2 3 4 5 6 7"
        putStrLn "============="

getRow :: Int -> [[TeamColour]] -> [Char]
getRow n [] = []
getRow n (col:restCol) = 
    if (n-1) < length col then
        show (col !! (n-1)) ++ " " ++ getRow n restCol
    else
        "* " ++ getRow n restCol

instance Eq TeamColour where
   c1 == c2 = show c1 == show c2
instance Show TeamColour where
   show Red = "X"
   show Black = "O"
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]



------- A Player -------

simplePlayer :: Player
-- simplePlayer has an ordering of the moves, and chooses the first one available
-- The order is it will choose the middle one first, then whatever is closest to the middle next
simplePlayer (State _ avail) = head [Action e | e <- [4, 3, 5, 2, 6, 1, 7],
                                               Action e `elem` avail]

-- Test cases


