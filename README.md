# Haskell-Tetris
This is my final project for EECS 368 at the University of Kansas. The project was to recreate a game using one of the two languages we learned throughout the semester. I chose to recreate the classic game `Tetris` using Haskell.

## Gameplay Demo
![Gameplay Demo](tetris-gameplay.mp4)

## Install
Assuming you have [GHC](https://www.haskell.org/ghc/) and [stack](https://docs.haskellstack.org/en/stable/README/) installed, all you need to do is clone the repository and install the packages using stack:
```
git clone
cd Haskell-Tetris/
stack install
```

To run it, use stack as well:
```
stack exec Haskell-Tetris-exe
```

This will run a web server on port 3000, to play the game, open any browser and navigate to [the following address](http://localhost:3000):
```
http://localhost:3000
```

## Controls
| Key             | Action                                              |
| --------------- | --------------------------------------------------- |
| Left Arrow Key  | Move the tetromino left                             |
| Right Arrow Key | Move the tetromino right                            |
| Down Arrow Key  | Hard drop the tetromino (instantly drops the piece) |
| Q               | Rotate tetromino counter clockwise                  |
| E               | Rotate tetromino clockwise                          |

## Additional Info
This project taught me a lot, and throughout the creation of this project I really started to enjoy coding in Haskell. It's a completely differet type of coding than I was used to in regular OOP style coding in C/C++.

Thinking in terms of functions, function compositions, list comprehensions, and mondas instead of the usual objects was a fun challenge, and I hope to do more with the language in the future.

### Things I'm most proud of
On a whole, I'm proud of this project in general. Making Tetris is not an easy task, and I think I did a genuinely good job. If I were to point out particular parts though, I am most proud of the following functionalities:

```haskell
mapBoard :: Matrix -> ((Int, Int) -> GridSquare) -> Matrix
```

`mapBoard` is a higher order function that takes a board matrix, and a function that takes a two tuple for a position on the board and returns a GridSquare, and will result in a manipulated matrix. This is used to change the game board based off of the position of the piece. For example, we use mapBoard when having a piece fall down. We get the position of all the falling pieces, and if the position below of any of those is the one we're evaluating, then we set that to a falling piece. If the position was one of the falling pieces, we set that to empty, since the piece will have fallen.

```haskell
placePiece :: [(Int, Int)] -> GridSquare -> Matrix -> Matrix
```

`placePiece` is a function that takes an array of positions, a square to add, and the board and it adds the piece to the board. where the array of positions is the pattern for a piece. This way you can easily place any piece on the board using an array of positions starting at (0,0) easily using this function.

## Things I want to add
If I finish this project (after the due date, just for fun), there's a list of features I wanted to add to the game. These are features that are in the main tetris game that I didn't have time or want to dedicate time to implement:
- [ ] Soft dropping (faster falling so a piece isn't immediately set)
- [ ] Proper rotation, right now the center of rotation is calculated each time a piece is rotated. This leads to the piece slowly drifting to the side, or falling faster after each rotation. What would be ideal is to store the center of rotation as a part of the GridSquare data class, that way the rotation is actually around a center not just jankily slapped around.
 - [ ] Scoring system, there's no score, I didn't want to figure out how to score the game so I just kind of didn't
 - [ ] Holding, allowing players to hold on to a piece to use later
 - [ ] Piece queue, allowing players to see what pieces are coming next so they can make better judgement on where to place their current pieces