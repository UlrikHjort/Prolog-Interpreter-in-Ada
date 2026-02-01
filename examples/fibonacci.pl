% Fibonacci Numbers
% Various implementations of the Fibonacci sequence.

% Simple recursive (inefficient - exponential time)
fib_simple(0, 0).
fib_simple(1, 1).
fib_simple(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_simple(N1, F1),
    fib_simple(N2, F2),
    F is F1 + F2.

% With accumulator (linear time)
fib(N, F) :- fib_acc(N, 0, 1, F).

fib_acc(0, A, _, A) :- !.
fib_acc(N, A, B, F) :-
    N > 0,
    N1 is N - 1,
    Sum is A + B,
    fib_acc(N1, B, Sum, F).

% Generate first N Fibonacci numbers
fib_list(N, List) :- fib_list_acc(N, 0, 1, [], List).

fib_list_acc(0, _, _, Acc, Acc) :- !.
fib_list_acc(N, A, B, Acc, List) :-
    N > 0,
    N1 is N - 1,
    Sum is A + B,
    append(Acc, [A], NewAcc),
    fib_list_acc(N1, B, Sum, NewAcc, List).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Check if N is a Fibonacci number
is_fib(N) :- is_fib_check(N, 0, 1).

is_fib_check(N, N, _) :- !.
is_fib_check(N, A, B) :-
    A < N,
    Sum is A + B,
    is_fib_check(N, B, Sum).

% Example queries:
% ?- fib(10, F).          % 10th Fibonacci number
% ?- fib_list(10, L).     % First 10 Fibonacci numbers
% ?- is_fib(21).          % Is 21 a Fibonacci number?
