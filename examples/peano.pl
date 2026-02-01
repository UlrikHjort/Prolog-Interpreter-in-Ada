% Peano Arithmetic
% Natural numbers represented as: 0, s(0), s(s(0)), s(s(s(0))), ...

% Natural number definition
nat(0).
nat(s(X)) :- nat(X).

% Addition: add(X, Y, Z) means X + Y = Z
add(0, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% Multiplication: mult(X, Y, Z) means X * Y = Z
mult(0, _, 0).
mult(s(X), Y, Z) :-
    mult(X, Y, W),
    add(Y, W, Z).

% Factorial
factorial(0, s(0)).
factorial(s(N), F) :-
    factorial(N, F1),
    mult(s(N), F1, F).

% Less than
less(0, s(_)).
less(s(X), s(Y)) :- less(X, Y).

% Less than or equal
leq(0, _).
leq(s(X), s(Y)) :- leq(X, Y).

% Subtraction (partial): sub(X, Y, Z) means X - Y = Z (only if X >= Y)
sub(X, 0, X).
sub(s(X), s(Y), Z) :- sub(X, Y, Z).

% Convert between Peano and integer
peano_to_int(0, 0).
peano_to_int(s(P), N) :-
    peano_to_int(P, N1),
    N is N1 + 1.

int_to_peano(0, 0) :- !.
int_to_peano(N, s(P)) :-
    N > 0,
    N1 is N - 1,
    int_to_peano(N1, P).

% Example queries:
% ?- add(s(s(0)), s(s(s(0))), X).  % 2 + 3 = ?
% ?- mult(s(s(0)), s(s(s(0))), X). % 2 * 3 = ?
% ?- factorial(s(s(s(0))), F).     % 3! = ?
