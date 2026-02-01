% Factorial
% Different implementations of factorial.

% Simple recursive
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% With accumulator (tail recursive)
factorial_acc(N, F) :- factorial_acc(N, 1, F).

factorial_acc(0, Acc, Acc) :- !.
factorial_acc(N, Acc, F) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial_acc(N1, Acc1, F).

% Double factorial: n!! = n * (n-2) * (n-4) * ... * 1 or 2
double_factorial(0, 1) :- !.
double_factorial(1, 1) :- !.
double_factorial(N, F) :-
    N > 1,
    N2 is N - 2,
    double_factorial(N2, F2),
    F is N * F2.

% Binomial coefficient: C(n,k) = n! / (k! * (n-k)!)
binomial(N, 0, 1) :- N >= 0, !.
binomial(N, N, 1) :- N >= 0, !.
binomial(N, K, C) :-
    N > 0, K > 0, K < N,
    N1 is N - 1,
    K1 is K - 1,
    binomial(N1, K1, C1),
    binomial(N1, K, C2),
    C is C1 + C2.

% Example queries:
% ?- factorial(10, F).        % 10! = 3628800
% ?- factorial_acc(10, F).    % Same but tail recursive
% ?- double_factorial(7, F).  % 7!! = 105
% ?- binomial(5, 2, C).       % C(5,2) = 10
