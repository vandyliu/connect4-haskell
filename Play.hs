-- CPSC 312 - 2021 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import Connect4
import Players

import System.IO
import Text.Read   (readMaybe)

type TournamentState = (Int,Int,Int)   -- wins, losses, ties


play :: Game -> State -> Player -> TournamentState -> IO TournamentState
play game start_state opponent ts =
  let (wins, losses,ties) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            personPlay game (ContinueGame start_state) opponent ts
        else if line ==  "1"
             then computerPlay game (ContinueGame start_state) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

-- opponent has played, the person must now play
personPlay :: Game -> Result -> Player -> TournamentState -> IO TournamentState

personPlay game (ContinueGame state) opponent ts =
   do
      let State internal avail = state
      let (slots, colour, board) = internal
      printBoard board
      putStrLn ("Choose one of "++show avail++" as "++show colour)
      line <- getLine
      case (readMaybe line :: Maybe Action) of
        Nothing ->
           personPlay game (ContinueGame state) opponent ts
        Just action ->
           if (action `elem` avail)
             then
                computerPlay game (game action state) opponent ts
             else
               do
                putStrLn ("Illegal move: "++ show action)
                personPlay game (ContinueGame state) opponent ts

personPlay game (EndOfGame val end_board start_state) opponent ts =
  do
    newts <- finishGame end_board (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

-- computerPlay game current_result opponent ts
-- person has played, the computer must now play
computerPlay :: Game -> Result -> Player -> TournamentState -> IO TournamentState
computerPlay game (EndOfGame val end_board start_state) opponent ts =
   do
      newts <- finishGame end_board val ts
      play game start_state opponent newts

computerPlay game (ContinueGame state) opponent ts =
      let 
          State internal avail = state
          (slots, colour, board) = internal
        in
          do
            opponent_move <- opponent state
            putStrLn ("The computer (" ++ show colour ++ ") chose " ++ show opponent_move)
            personPlay game (game opponent_move state) opponent ts

-- computerMove does a computer move for the graphics version
computerMove :: Game -> Result -> Player -> IO Result
computerMove game (EndOfGame val finishedState initialState) opponent = return (EndOfGame val finishedState initialState)

computerMove game (ContinueGame state) opponent = 
      let 
          State internal avail = state
          (slots, colour, board) = internal
        in
          do
            opponent_move <- opponent state
            return (connect4 opponent_move state)

finishGame :: State -> Double -> TournamentState -> IO TournamentState
finishGame (State (_, _, end_board) _) val (wins,losses,ties) = 
    do 
        putStrLn("Game Over. Final Board.")
        printBoard end_board
        updateTournamentState val (wins,losses,ties)

-- given value to the person, the tournament state, return the new tournament state
updateTournamentState :: Double -> TournamentState -> IO TournamentState
updateTournamentState val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)
