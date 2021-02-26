# connect4-haskell

NOTE: 
Play.hs sourced from "https://www.cs.ubc.ca/~poole/cs312/2021/haskell/Play.hs", and edited to display our connect 4 game.

To Play:

`:load Play`

To play against simple computer :

`play connect4 connect4Start simplePlayer (0,0,0)`

To play against Monte-Carlo algorithm computer (Substitute X for number of games to play out at each move):

`play connect4 connect4Start (monteCarloPlayer X) (0,0,0)`