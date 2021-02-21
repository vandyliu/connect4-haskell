-- CPSC 312 - 2021 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Connect4

import System.IO
import Text.Read   (readMaybe)

type TournammentState = (Int,Int,Int)   -- wins, losses, ties


play :: Game -> State -> Player -> TournammentState -> IO TournammentState

play game start_state opponent ts =
  let (wins, losses,ties) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game (ContinueGame start_state) opponent ts
        else if line ==  "1"
             then computer_play game (ContinueGame start_state) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play

person_play game (ContinueGame state) opponent ts =
   do
      let State internal avail = state
      putStrLn ("State: "++show internal++" choose one of "++show avail)
      line <- getLine
      case (readMaybe line :: Maybe Action) of
        Nothing ->
           person_play game (ContinueGame state) opponent ts
        Just action ->
           if (action `elem` avail)
             then
                computer_play game (game action state) opponent ts
             else
               do
                putStrLn ("Illegal move: "++ show action)
                person_play game (ContinueGame state) opponent ts

person_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val  start_state) opponent ts =
   do
      newts <- update_tournament_state val ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent ts =
      let 
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game opponent_move state) opponent ts

update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)

-- If you imported MagicSum here and in Minimax try:
-- play magicsum magicsum_start simple_player (0,0,0)
-- play magicsum magicsum_start (mm_player magicsum) (0,0,0) -- minimax player

-- If you imported CountGameNew here and in Minimax_mem try:
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (simple_count_player 20 [1,2,3,5,7]) (0,0,0)
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (mm_player cg) (0,0,0)