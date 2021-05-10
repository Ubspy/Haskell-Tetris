# Haskell-Tetris
This is my final project for EECS 368 at the University of Kansas. The project was to recreate a game using one of the two languages we learned throughout the semester. I chose to recreate the classic game `Tetris` using Haskell.

## Gameplay Demo
![Gameplay Demo](tetris-gameplay.gif)

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
TBA