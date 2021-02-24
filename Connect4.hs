module Connect4 where

import TreeDict

import System.IO
import Data.List
import System.Random

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Eq, Show, Ord)

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
    deriving Ord

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

connect4State1 :: State
-- connect4State1 goes to a board state
connect4State1 = State (17, Red, [[Black, Red, Black],[Black, Red, Black, Red, Red],[Red, Black],[Red, Black, Red, Black, Red, Black], [Red, Black],[Black, Red, Black, Red, Red],[Black]]) [Action n | n <- [1,2,3,5,6,7]]

connect4State2 :: State
-- connect4State2 goes to a board state
connect4State2 = State (9, Red, [[Black, Black, Black, Red, Red, Red],[Red,Red,Black,Red,Black,Red],[Black],[Red,Black,Red,Black,Red,Red],[Black,Red,Black,Red,Black,Black],[Black],[Black,Red,Black,Red,Black,Red]]) [Action n | n <- [3,6]]

connect4State3 :: State
-- connect4State3 goes to a board state
connect4State3 = State (17, Red, [[Black, Black, Black, Red, Red, Red],[Red,Red,Black],[],[Red,Black,Red,Black,Red,Red],[Black,Red,Black,Red,Black,Black],[Black],[Black,Red]]) [Action n | n <- [2,3,6,7]]

connect4State4 :: State
-- connect4State4 goes to a board state
connect4State4 = State (9, Red, [[Black, Red, Red, Black],[Red,Red,Black,Red,Red,Black],[],[Red,Black,Red,Black,Red,Black],[Black,Red,Black,Red,Black,Black],[Red,Black,Black,Red],[Red,Black,Red,Black,Red,Black]]) [Action n | n <- [1,3,6]]

printBoard :: [[TeamColour]] -> IO ()
-- Print the board to the output, where "X" represents Red, "O" represents Black, and "-" represents an empty space
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



------- A Player -------

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

--------- MiniMax Player ---------
type Mem = Dict (State,Action) Double

----   Determining the best move  ---
minimax:: Game -> State -> Mem -> ((Action, Double), Mem)
-- minimax game state memory  =>  ((move,value_to_player),new_memory)
minimax game st mem =
   argmax_mem (valueact game st) avail mem
   where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact :: Game -> State -> Action -> Mem -> (Double,Mem)
valueact game st act mem =
     case getval (st,act) mem of
         Just val -> (val,mem)
         Nothing ->
            let (val,mem1) =
                   value game (game act st) mem
            in (val, insertval (st,act) val mem1)


-- value game move result = value for current player of the state after move given result
value:: Game -> Result -> Mem -> (Double,Mem)
value _  (EndOfGame val _ _) mem = (val, mem)
value game (ContinueGame st) mem =
       let ((_,val), mem2) = minimax game st mem
          in  (-val,mem2)

-- argmax_mem f lst mem = ((e, f e), mem1)
-- where e is an element of lst that has a maximal value for f
--  updates the memory to mem1. Each call to f can update memory
argmax_mem :: Ord v => (e -> m -> (v,m)) -> [e] -> m -> ((e,v),m)
argmax_mem f [e] mem = ((e, v),mem1)
     where (v,mem1) = f e mem
argmax_mem f (h:t) mem
   | fh > ft = ((h,fh),mem2)
   | otherwise = ((bt, ft),mem2)
   where
      ((bt,ft),mem1) = argmax_mem f t mem
      (fh,mem2) = f h mem1

{-
--with import CountGame
let (cg, is) = createCountGame 20 [1,2,3,5,7] in minimax cg is emptyDict 
let (cg, is) = createCountGame 21 [1,2,3,5,7] in minimax cg is emptyDict 
let (cg, is) = createCountGame 100 [1..9] in minimax cg is emptyDict 
let (cg, is) = createCountGame 101 [2,4,10,14] in minimax cg is emptyDict 
let (cg, is) = createCountGame 101 [3,5,13,17,37] in minimax cg is emptyDict
-}

-- For MagicSum
-- to find the best opening move
-- res = minimax magicsum magicsum_start emptyDict
-- (fst res)
-- stats (snd res)   -- gets the size and depth of the memory

--Try
-- as = map Action     -- make it easier to type
-- minimax magicsum (State (as [8,5,4], as [1,2,6,9])  (as [3,7])) emptyDict
-- minimax magicsum (State (as [2,6,9], as [8,5,4])  (as [1,3,7])) (snd it)
-- minimax magicsum (State (as [5,4], as [2,6,9])  (as [1,3,7,8])) (snd it)
-- stats (snd it)

mm_player:: Game -> Player
mm_player game state = do
    let action = fst (fst ( minimax game state emptyDict))
    return action

{-

--with import MagicSum and TreeDict (at the top of this file):
:set +s
mm = minimax magicsum magicsum_start emptyDict
fst mm
stats (snd mm)  
"Number of elements=294778, Depth=103"
mm2 = minimax magicsum magicsum_start (snd mm)
fst mm2

--with import MagicSum_ord and TreeDict:
mmo = minimax magicsum magicsum_start emptyDict
fst mmo
stats $ snd mmo  -- returns
"Number of elements=4520, Depth=52"
-}

mmPlayer :: Player
mmPlayer st = mm_player connect4 st

mmPlayerHybrid :: Player
mmPlayerHybrid st = 
    let State is avail = st
        (movesLeft, _, _) = is 
    in if length avail < 3 then mmPlayer st else randomPlayer st
-- Needs to be tested more after win function is complete

-- Should create heuristic



decentPlayer :: Player
-- check if any action leads to win
-- check if any action for opponent leads to win and block it
-- check if action leads to a play that will lead to a win
-- pick middlest action available out of above actions 