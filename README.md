# connect4-haskell

NOTE: 
Play.hs sourced from "https://www.cs.ubc.ca/~poole/cs312/2021/haskell/Play.hs", and edited to display our connect 4 game.

To set up:

```
stack install random
stack install hashable
stack ghci
```

To Play:

`:load Play`

To play against simple computer :

`play connect4 connect4Start simplePlayer (0,0,0)`

To play against Monte-Carlo algorithm computer:
- substitute X for number of games to play out at each move

`play connect4 connect4Start (monteCarloPlayer X) (0,0,0)`

To play against a Hybrid Monte-Carlo algorithm - MiniMax computer:
- substitute X for number of games to play out at each move for Monte-Carlo computer
- substitute Y for amount of moves left to switch to MiniMax computer

`play connect4 connect4Start (hybridPlayer Y X) (0,0,0)`