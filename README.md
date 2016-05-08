# __f's and &lambda;'s__

_f's and &lambda;'s_ is a state-free implementation of tic-tac-toe. Since
everything it does is functionally pure (aside from printing to the terminal),
it can't actually remember the board between turns. Look below for instructions
on how to play ~~(although I don't know why anyone would want to)~~.

## Playing
- Start up [Racket](https://racket-lang.org/) by running `racket` in the project directory
- Import the player-accessible functions with `(require "fal.rkt")`
- Take turns calling the functions `play-x` and `play-o` until the game is over

Each function takes two parameters: the position to be played and the previous
board state. The position is a string corresponding to the square you wish to
play in, e.g. `"b3"`. The full array of positions is:
```
a1 | a2 | a3
---|----|---
b1 | b2 | b3
---|----|---
c1 | c2 | c3
```
The state is a three-character string that is printed out along with each board.
An empty board corresponds to the string `"000"`, so use this as the state for
the first move.

### Example
To begin a game by playing an `X` in the center square, the first
player calls `(play-x "b2" "000")`. This produces the following output:
```
BOARD CODE: 0x0
   |   |  
---|---|---
   | X |  
---|---|---
   |   |  
```
Since the resulting board has the code `"0x0"`, the second player uses this as
their state input. For example, they might respond by playing an `O` in the
bottom right by calling `(play-o "c3" "0x0")`. This produces:
```
BOARD CODE: 0xy
   |   |  
---|---|---
   | X |  
---|---|---
   |   | O
```
The game continues on in this way until a player gets three symbols in a row or
(if this never happens) until all spaces fill up, at which point the game is
declared a draw.
