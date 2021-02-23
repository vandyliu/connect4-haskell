-- CPSC 312 - 2021 - Games in Haskell
-- Minimax with Memory, remembers the Q-function, Q(s,a), as in Q-learning
module MiniMax where

-- To run it, try:
-- ghci
-- :load Minimax_mem

-- Uncomment one of the following
-- import MagicSum
-- import MagicSum_ord
-- import CountGame
-- import CountGameNew
import Connect4

-- Uncomment one of the following
import TreeDict
--import FunDict

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
mm_player game state = fst (fst ( minimax game state emptyDict))

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