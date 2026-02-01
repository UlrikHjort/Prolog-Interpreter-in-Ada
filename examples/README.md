# Prolog Examples

This directory contains example Prolog programs demonstrating various concepts and algorithms.

## Classic Problems

- **8queens.pl** - The N-Queens problem (placing queens on a chessboard)
- **hanoi.pl** - Towers of Hanoi puzzle
- **knight.pl** - Knight's tour on a chessboard
- **puzzle.pl** - Logic puzzles including Zebra/Einstein riddle
- **sudoku.pl** - 4x4 Sudoku solver
- **cryptarith.pl** - Cryptarithmetic puzzles (SEND+MORE=MONEY)
- **mapcolor.pl** - Graph/map coloring problem

## Data Structures

- **lists.pl** - Comprehensive list operations
- **btree.pl** - Binary tree operations and traversals
- **sets.pl** - Set operations (union, intersection, etc.)
- **matrix.pl** - Matrix operations (add, multiply, transpose)
- **graph.pl** - Graph algorithms (path finding, connectivity)

## Algorithms

- **quicksort.pl** - Quicksort algorithm
- **mergesort.pl** - Mergesort algorithm
- **search.pl** - DFS, BFS, iterative deepening
- **primes.pl** - Prime number operations

## Mathematical

- **factorial.pl** - Factorial with different implementations
- **fibonacci.pl** - Fibonacci sequence
- **peano.pl** - Peano arithmetic (natural numbers)
- **symbolic.pl** - Symbolic differentiation
- **float.pl** - Floating-point arithmetic examples

## Other Examples

- **family.pl** - Family relationships (classic Prolog example)
- **database.pl** - Relational database-style queries
- **automata.pl** - Finite automata (DFA, NFA)
- **dcg.pl** - Natural language parsing
- **logic.pl** - Propositional logic evaluation
- **expert.pl** - Simple expert system
- **games.pl** - Tic-tac-toe and simple games

## Running Examples

Load an example file and run queries:

```
./bin/prolog examples/factorial.pl
?- factorial(10, F).
F = 3628800
```

Or interactively:
```
./bin/prolog
?- consult('examples/lists.pl').
?- reverse([1, 2, 3], R).
R = [3, 2, 1]
```

## Notes

- Some examples (like 8-queens with N=8, knight's tour) can be slow due to the search space
- Use smaller problem sizes for testing (e.g., 4-queens instead of 8-queens)
- Not all standard Prolog built-ins are available; see the interpreter documentation

For more detailed information about each example, see [docs/examples.md](../docs/examples.md).
