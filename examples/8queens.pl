% 8 Queens Problem
% Place 8 queens on a chessboard so that no two queens attack each other.

% A solution is a permutation of [1,2,3,4,5,6,7,8] where the position
% in the list represents the column and the value represents the row.

queens(N, Qs) :-
    numlist(1, N, Rows),
    permutation(Rows, Qs),
    safe(Qs).

% Check if all queens are safe (no diagonal attacks)
safe([]).
safe([Q|Qs]) :-
    safe(Qs),
    no_attack(Q, Qs, 1).

% Check that Q doesn't attack any queen in Qs
no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :-
    Q1 - Q =\= D,
    Q - Q1 =\= D,
    D1 is D + 1,
    no_attack(Q, Qs, D1).

% Generate a list [1, 2, ..., N]
numlist(N, N, [N]) :- !.
numlist(M, N, [M|Rest]) :-
    M < N,
    M1 is M + 1,
    numlist(M1, N, Rest).

% Permutation predicate
permutation([], []).
permutation(List, [H|Perm]) :-
    select(H, List, Rest),
    permutation(Rest, Perm).

% Select an element from a list
select(X, [X|T], T).
select(X, [H|T], [H|R]) :-
    select(X, T, R).

% Example query: Find a solution for 4 queens (faster than 8)
% ?- queens(4, Q).
