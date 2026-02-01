# Built-in Predicates Reference

This document describes all built-in predicates available in the Ada Prolog Interpreter.

## Control Predicates

| Predicate | Description |
|-----------|-------------|
| `true` | Always succeeds |
| `fail` | Always fails |
| `!` | Cut - commits to current choice, pruning backtrack points |
| `\+ Goal` | Negation as failure - succeeds if Goal fails |

### Examples

```prolog
?- true.
true.

?- fail.
false.

% Cut prevents backtracking
first_member(X, [X|_]) :- !.
first_member(X, [_|T]) :- first_member(X, T).

% Negation as failure
not_member(X, L) :- \+ member(X, L).
```

## Unification

| Predicate | Description |
|-----------|-------------|
| `X = Y` | Unifies X with Y |
| `X \= Y` | Succeeds if X and Y do not unify |

### Examples

```prolog
?- X = foo(1, 2).
X = foo(1, 2)

?- foo(X, 2) = foo(1, Y).
X = 1
Y = 2

?- 1 \= 2.
true.
```

## Arithmetic

### Evaluation

| Predicate | Description |
|-----------|-------------|
| `X is Expr` | Evaluates Expr and unifies result with X |

### Operators

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction (binary) or negation (unary) |
| `*` | Multiplication |
| `/` | Division (returns float if not evenly divisible) |
| `mod` | Modulo |
| `**` | Exponentiation |

### Functions

| Function | Description |
|----------|-------------|
| `abs(X)` | Absolute value |
| `sqrt(X)` | Square root |
| `floor(X)` | Floor (round down) |
| `ceiling(X)` | Ceiling (round up) |
| `round(X)` | Round to nearest integer |
| `truncate(X)` | Truncate toward zero |
| `float(X)` | Convert to float |

### Examples

```prolog
?- X is 3 + 4 * 2.
X = 11

?- X is 10 / 4.
X = 2.5

?- X is sqrt(16.0).
X = 4

?- X is 2 ** 10.
X = 1024

?- X is floor(3.7).
X = 3
```

## Arithmetic Comparison

| Predicate | Description |
|-----------|-------------|
| `X =:= Y` | Arithmetic equality |
| `X =\= Y` | Arithmetic inequality |
| `X < Y` | Less than |
| `X > Y` | Greater than |
| `X =< Y` | Less than or equal |
| `X >= Y` | Greater than or equal |

### Examples

```prolog
?- 3 + 4 =:= 7.
true.

?- 5 > 3.
true.

?- 2.5 =< 3.0.
true.
```

## Type Checking

| Predicate | Description |
|-----------|-------------|
| `var(X)` | Succeeds if X is an unbound variable |
| `nonvar(X)` | Succeeds if X is not an unbound variable |
| `atom(X)` | Succeeds if X is an atom |
| `integer(X)` | Succeeds if X is an integer |
| `float(X)` | Succeeds if X is a floating-point number |
| `number(X)` | Succeeds if X is a number (integer or float) |
| `compound(X)` | Succeeds if X is a compound term |
| `is_list(X)` | Succeeds if X is a proper list |

### Examples

```prolog
?- var(X).
true.

?- X = 5, nonvar(X).
true.

?- atom(hello).
true.

?- integer(42).
true.

?- float(3.14).
true.

?- number(3.14).
true.

?- compound(foo(1, 2)).
true.
```

## Term Manipulation

| Predicate | Description |
|-----------|-------------|
| `functor(T, F, A)` | T has functor F and arity A |
| `arg(N, T, A)` | A is the Nth argument of T (1-indexed) |
| `T =.. L` | Univ - T is composed from list L = [Functor\|Args] |
| `copy_term(T, C)` | C is a copy of T with fresh variables |

### Examples

```prolog
?- functor(foo(a, b, c), F, A).
F = foo
A = 3

?- functor(T, bar, 2).
T = bar(_G1, _G2)

?- arg(2, foo(a, b, c), X).
X = b

?- foo(1, 2) =.. L.
L = [foo, 1, 2]

?- T =.. [bar, x, y].
T = bar(x, y)
```

## Input/Output

| Predicate | Description |
|-----------|-------------|
| `write(X)` | Writes X to standard output |
| `writeln(X)` | Writes X followed by newline |
| `nl` | Writes a newline |

### Examples

```prolog
?- write('Hello'), nl, write('World').
Hello
World
true.

?- writeln(foo(1, 2)).
foo(1, 2)
true.
```

## Database Manipulation

| Predicate | Description |
|-----------|-------------|
| `asserta(C)` | Adds clause C at the beginning of the database |
| `assertz(C)` | Adds clause C at the end of the database |
| `assert(C)` | Same as assertz |

### Examples

```prolog
?- assertz(fact(1)).
true.

?- assertz(fact(2)).
true.

?- fact(X).
X = 1
; X = 2
```

## File Operations

| Predicate | Description |
|-----------|-------------|
| `consult(File)` | Loads and executes clauses from File |

### Examples

```prolog
?- consult('examples/lists.pl').
% examples/lists.pl consulted.
true.
```
