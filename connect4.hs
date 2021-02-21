module Connect4 where

import System.IO

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

------ Connect4 Game -------
data Action = Action Int                 -- a move for a player is just an Int
         deriving (Ord,Eq)

data TeamColour = Red
                | Black
type InternalState = (Int, TeamColour, [[TeamColour]])   -- (open slots remaining, current colour's turn, 2d array of BoardSpace)


connect4 :: Game
connect4 move (State (remaining, colour, board) available_actions) 
    | win move (remaining, colour, board)     = EndOfGame 1  connect4_start   -- agent wins
    | remaining == 1               = EndOfGame 0  connect4_start   -- no more moves, tie
    | otherwise                    =
          ContinueGame (State (remaining - 1, Red, board)   -- TODO: should switch to other colour and update board
                        available_actions)                  -- TODO: should remove column from available_actions if column is full

-- win n internalState = the agent wins if it selects a column that will leave four pieces of their colour in a line either horizontally, vertially, or diagaonlly
win :: Action -> InternalState -> Bool
win (Action n) int_state = False -- TODO: determine if move leads to winning state for current colour


connect4_start = State (42, Red,[[]]) [Action n | n <- [1..7]] -- Red will Start, board is originally empty

instance Eq TeamColour where
   c1 == c2 = show c1 == show c2
instance Show TeamColour where
   show Red = "R"
   show Black = "B"
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]



------- A Player -------

simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player (State _ avail) = head [Action e | e <- [1,2,3,4,5,6,7],
                                               Action e `elem` avail]


-- Test cases


