# Monadic Sudoku Solver

A simple sudoku solver, implemented using monad transformers

### Prerequisites

Requires dequeue-0.1.12

### How to Run

You can either compile the project first before running it, or
you can run it in the ghci toploop directly.

## Using the solver

Run ```solveSudoku <initial_board>```. The board needs to be a map with 
key type ```(Integer, Integer)``` and value type ```Integer```.

Example: ```let testBoard = M.fromList [((0,0),1),((0,1),2),((0,2),0),((0,3),0),
                                        ((1,0),3),((1,1),0),((1,2),0),((1,3),0),
                                        ((2,0),0),((2,1),0),((2,2),0),((2,3),3),
                                        ((3,0),0),((3,1),0),((3,2),0),((3,3),1)]
            in solveSudoku testBoard
                                        ```

## Notes

This solver currently only works for 2x2 puzzle; there's a bug in the code that
will cause the program to crash on bigger puzzles. I am currently working on
fixing this bug.
