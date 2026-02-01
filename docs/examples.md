# Examples Guide

This document describes the example programs included with the Ada Prolog Interpreter.

## Running Examples

```bash
# Load an example file
./bin/prolog examples/factorial.pl

# Then run queries
?- factorial(10, X).
X = 3628800
```

## Example Programs

### Classic Algorithms

#### factorial.pl
Recursive factorial computation.

```prolog
?- factorial(5, X).
X = 120
```

#### fibonacci.pl
Fibonacci sequence generator.

```prolog
?- fib(10, X).
X = 55
```

#### quicksort.pl / mergesort.pl
Sorting algorithms using list operations.

```prolog
?- quicksort([3,1,4,1,5,9,2,6], Sorted).
Sorted = [1, 1, 2, 3, 4, 5, 6, 9]
```

#### primes.pl
Prime number generation and testing.

```prolog
?- prime(17).
true.

?- primes_up_to(20, Ps).
Ps = [2, 3, 5, 7, 11, 13, 17, 19]
```

### Puzzles and Games

#### 8queens.pl
The classic 8-queens chess puzzle.

```prolog
?- queens(8, Qs).
Qs = [1, 5, 8, 6, 3, 7, 2, 4]
```

#### hanoi.pl
Tower of Hanoi puzzle solver.

```prolog
?- hanoi(3).
Move disk from a to c
Move disk from a to b
Move disk from c to b
Move disk from a to c
Move disk from b to a
Move disk from b to c
Move disk from a to c
```

#### knight.pl
Knight's tour on a chess board.

```prolog
?- knight_tour(5, Tour).
```

#### sudoku.pl
Sudoku puzzle solver.

```prolog
?- sudoku(Puzzle, Solution).
```

#### cryptarith.pl
Cryptarithmetic puzzle solver (SEND + MORE = MONEY).

```prolog
?- solve([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]).
```

#### puzzle.pl
Various logic puzzles.

#### games.pl
Simple game implementations.

### Data Structures

#### lists.pl
Comprehensive list operations.

```prolog
?- append([1,2], [3,4], X).
X = [1, 2, 3, 4]

?- reverse([1,2,3], X).
X = [3, 2, 1]

?- length([a,b,c,d], N).
N = 4
```

#### btree.pl
Binary tree operations.

```prolog
?- insert(5, nil, T1), insert(3, T1, T2), insert(7, T2, T3).
T3 = node(5, node(3, nil, nil), node(7, nil, nil))
```

#### sets.pl
Set operations using lists.

```prolog
?- union([1,2,3], [2,3,4], U).
U = [1, 2, 3, 4]

?- intersection([1,2,3], [2,3,4], I).
I = [2, 3]
```

#### matrix.pl
Matrix operations.

```prolog
?- transpose([[1,2],[3,4],[5,6]], T).
T = [[1, 3, 5], [2, 4, 6]]
```

#### graph.pl
Graph algorithms (paths, cycles, connectivity).

```prolog
?- path(a, d, Path).
Path = [a, b, c, d]
```

### Logic and AI

#### search.pl
Search algorithms (DFS, BFS, iterative deepening).

```prolog
?- dfs(start, goal, Path).
```

#### expert.pl
Simple expert system shell.

```prolog
?- diagnose(Disease).
```

#### logic.pl
Propositional logic operations.

```prolog
?- tautology(implies(and(P,Q), P)).
true.
```

#### automata.pl
Finite automata simulation.

```prolog
?- accepts([a,b,a,b]).
true.
```

### Specialized

#### family.pl
Family relationship reasoning (classic Prolog example).

```prolog
?- grandparent(X, ann).
X = tom
```

#### peano.pl
Peano arithmetic - natural numbers as successor terms.

```prolog
?- add(s(s(0)), s(s(s(0))), X).
X = s(s(s(s(s(0)))))
```

#### symbolic.pl
Symbolic differentiation.

```prolog
?- derivative(x*x + 2*x, x, D).
D = 2*x + 2
```

#### dcg.pl
Definite Clause Grammar examples.

```prolog
?- sentence([the, cat, chases, the, mouse], []).
true.
```

#### database.pl
Dynamic database manipulation examples.

```prolog
?- add_fact(likes(john, pizza)).
?- likes(john, What).
What = pizza
```

#### mapcolor.pl
Map coloring problem (constraint satisfaction).

```prolog
?- color_map(Colors).
Colors = [red, green, blue, red]
```

#### float.pl
Floating-point arithmetic examples.

```prolog
?- circle_area(5.0, A).
A = 78.54

?- quadratic_roots(1.0, -5.0, 6.0, X1, X2).
X1 = 3
X2 = 2
```
