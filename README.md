# Monadic Sudoku Solver

A simple sudoku solver, implemented using monad transformers

### Prerequisites

Requires dequeue-0.1.12

### How to Run

You can either compile the project first before running it, or
you can run it in the ghci toploop directly.

## Using the solver

Run ```solveSudoku <initial_board>```

## Notes

This solver currently only works for 2x2 puzzle; there's a bug in the code that
will cause the program to crash on bigger puzzles. I am currently working on
fixing this bug.
