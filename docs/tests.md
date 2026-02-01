# Test Suite Documentation

This document describes the test suite for the Ada Prolog Interpreter.

## Running Tests

```bash
# Run all tests
make test

# Or directly
./run_tests.sh
```

## Test Structure

Tests are stored in the `test/` directory as `.pl` files. Each test file contains:
1. Optional Prolog clauses (facts/rules)
2. A query to execute
3. The expected result is checked by the test runner

### Test File Format

```prolog
% Optional comment
optional_fact(value).
optional_rule(X) :- some_condition(X).
?- query_to_test.
```

### Test Runner

The `run_tests.sh` script:
1. Loads each test file into the interpreter
2. Captures the output
3. Checks if the expected result appears in the output
4. Reports pass/fail for each test

## Current Tests

### Basic Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Basic fact query | test_basic.pl | Simple fact lookup | `true` |
| Unification | test_unification.pl | same/2 predicate | `true` |

### Arithmetic Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Addition | test_arithmetic.pl | 3 + 4 = 7 | `X = 7` |
| Factorial | test_factorial.pl | factorial(5) = 120 | `X = 120` |
| Comparison | test_comparison.pl | max/3 predicate | `M = 5` |

### Floating-Point Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Float addition | test_float.pl | 3.14 + 1.86 = 5 | `X = 5` |
| Float division | test_float_div.pl | 10/4 = 2.5 | `true` |
| Float sqrt | test_float_sqrt.pl | sqrt(25) = 5 | `X = 5` |
| Float type check | test_float_type.pl | float/1, number/1 | `true` |

### List Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Append | test_lists.pl | append/3 | `X = [1, 2, 3, 4]` |
| Length | test_length.pl | length/2 | `N = 5` |
| Member found | test_member.pl | member/2 success | `true` |
| Member not found | test_notmember.pl | member/2 failure | `false` |

### Relation Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Grandparent | test_family.pl | grandparent relation | `true` |

### I/O Tests

| Test | File | Description | Expected |
|------|------|-------------|----------|
| Write | test_write.pl | write/1 output | `hello` |

## Test Details

### test_basic.pl
```prolog
foo(bar).
?- foo(bar).
```
Tests basic fact assertion and query.

### test_unification.pl
```prolog
same(X, X).
?- same(1, 1).
```
Tests unification of identical terms.

### test_arithmetic.pl
```prolog
?- X is 3 + 4.
```
Tests arithmetic evaluation.

### test_factorial.pl
```prolog
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
?- factorial(5, X).
```
Tests recursive computation with arithmetic.

### test_comparison.pl
```prolog
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.
?- max(3, 5, M).
```
Tests arithmetic comparison operators.

### test_float.pl
```prolog
?- X is 3.14 + 1.86.
```
Tests floating-point literal parsing and addition.

### test_float_div.pl
```prolog
?- 10 / 4 =:= 2.5.
```
Tests float division and comparison.

### test_float_sqrt.pl
```prolog
?- X is sqrt(25.0).
```
Tests square root function.

### test_float_type.pl
```prolog
?- float(3.14), number(3.14), \+ integer(3.14).
```
Tests type-checking predicates for floats.

### test_lists.pl
```prolog
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
?- append([1, 2], [3, 4], X).
```
Tests list append operation.

### test_length.pl
```prolog
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.
?- length([a, b, c, d, e], N).
```
Tests list length calculation.

### test_member.pl
```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
?- member(3, [1, 2, 3, 4]).
```
Tests successful member lookup.

### test_notmember.pl
```prolog
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
?- member(5, [1, 2, 3, 4]).
```
Tests failed member lookup.

### test_family.pl
```prolog
parent(tom, mary).
parent(mary, ann).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
?- grandparent(tom, ann).
```
Tests multi-step relation inference.

### test_write.pl
```prolog
?- write(hello).
```
Tests output predicate.

## Adding New Tests

1. Create a new `.pl` file in the `test/` directory:
   ```prolog
   % Description of what this tests
   ?- your_query_here.
   ```

2. Add the test to `run_tests.sh`:
   ```bash
   run_test "Test description" "$TESTS/test_new.pl" "expected output"
   ```

3. Run `make test` to verify.
