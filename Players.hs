module Players where

import Connect4
import System.Random
import HashTreeDict

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

monteCarloPlayer ::  Integer -> Player
-- monteCarloPlayer plays out each move X times by selecting random moves until completion, and selects the best outcome. (Checks if played out game ends in win or loss)
monteCarloPlayer numGames state = do
    putStrLn "MonteCarlo Player"
    actionValuePair <- mc connect4 state numGames
    return (fst actionValuePair)

-- For each action, play out X games and return the pair with the highest expected value (most wins)
mc:: Game -> State -> Integer -> IO (Action, Double)
mc game st numGames = do
    actionValuePairs <- getIOValues [ mcPlayOutXGames game st action numGames | action <- avail]
    return (argmax actionValuePairs)
    where State _ avail = st

-- Play out X games and return a pair with the action and sum of the played out results
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

-- Given a list of IO values, extract the IO values and return in a list
getIOValues :: [IO a] -> IO [a]
getIOValues [] = pure []
getIOValues (h:t) = do
    x <- h
    xs <- getIOValues t
    return (x:xs)

-- Given a list of pairs, return the pair with the highest value (second value of pair)
argmax :: Ord v => [(e,v)] -> (e,v)
argmax [(e,v)] = (e, v)
argmax (h:t) 
   | vh > vt = h
   | otherwise = (bt,vt)
   where
      (bt,vt) = argmax t
      vh = snd h

-- MiniMax Player --
type Mem = Dict State (Action, Double)

mmPlayer :: Player
mmPlayer st = do
    putStrLn "MiniMax Player"
    return (fst (fst (minimax connect4 st emptyDict)))

----   Determining the best move  ---
minimax:: Game -> State -> Mem -> ((Action, Double), Mem)
-- minimax game state memory  =>  ((move,value_to_player),new_memory)
minimax game st mem =
   case getval st mem of
      Just act_val  -> (act_val,mem)
      Nothing ->
        let (act_val,mem1) =
              argmaxMem (valueact game st) avail mem
        in (act_val, (insertval st act_val mem1))
    where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact :: Game -> State -> Action -> Mem -> (Double,Mem)
valueact game st act = value game (game act st) 


-- value game move result = value for current player of the state after move given result
value :: Game -> Result -> Mem -> (Double, Mem)
value _  (EndOfGame val (State (movesLeft,_,_) _) _) mem = (val * (fromIntegral movesLeft / 42), mem)
-- win quickly then you get 1 * (35/42) = 35/42
-- win slowly then you get 1 * (3/42) = 3/42
-- lose slowly then you get -1 * (3/42) = -3/42
-- lost quickly then you get -1 * (35/42) = -35/42
value game (ContinueGame st) mem =
       let ((_,val), mem2) = minimax game st mem
          in  (-val,mem2)

-- argmaxMem f lst mem = ((e, f e),mem1) 
--  updates the memory to mem1. Each call to f can uodate memory
argmaxMem :: Ord v => (e -> m -> (v,m)) -> [e] -> m -> ((e,v),m)
argmaxMem f [e] mem = ((e, v),mem1)
     where (v,mem1) = f e mem
argmaxMem f (h:t) mem
   | fh > ft = ((h,fh),mem2)
   | otherwise = ((bt, ft),mem2)
   where
      ((bt,ft),mem1) = argmaxMem f t mem
      (fh,mem2) = f h mem1

-- Hybrid Player --
hybridPlayer :: Int -> Integer -> Player
hybridPlayer mmMoves monteCarloGames st = 
    let State is avail = st
        (movesLeft, _, _) = is 
    in if movesLeft < mmMoves then mmPlayer st else monteCarloPlayer monteCarloGames st