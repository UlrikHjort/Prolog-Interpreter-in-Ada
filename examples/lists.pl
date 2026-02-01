% List Operations
% Comprehensive list manipulation predicates.

% Length of a list
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Append two lists
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Reverse a list
reverse([], []).
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% Reverse with accumulator (more efficient)
reverse_acc(L, R) :- reverse_acc(L, [], R).
reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).

% Member check
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Last element
last([X], X).
last([_|T], X) :- last(T, X).

% Nth element (1-indexed)
nth(1, [H|_], H) :- !.
nth(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth(N1, T, X).

% Take first N elements
take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, R).

% Drop first N elements
drop(0, L, L) :- !.
drop(_, [], []) :- !.
drop(N, [_|T], R) :-
    N > 0,
    N1 is N - 1,
    drop(N1, T, R).

% Sum of list elements
sum([], 0).
sum([H|T], S) :- sum(T, S1), S is H + S1.

% Product of list elements
product([], 1).
product([H|T], P) :- product(T, P1), P is H * P1.

% Maximum element
max_list([X], X) :- !.
max_list([H|T], Max) :-
    max_list(T, MaxT),
    (H > MaxT -> Max = H ; Max = MaxT).

% Minimum element
min_list([X], X) :- !.
min_list([H|T], Min) :-
    min_list(T, MinT),
    (H < MinT -> Min = H ; Min = MinT).

% Flatten nested lists
flatten([], []) :- !.
flatten([H|T], Flat) :-
    is_list(H), !,
    flatten(H, FH),
    flatten(T, FT),
    append(FH, FT, Flat).
flatten([H|T], [H|Flat]) :-
    flatten(T, Flat).

is_list([]).
is_list([_|_]).

% Remove duplicates
remove_dups([], []).
remove_dups([H|T], R) :-
    member(H, T), !,
    remove_dups(T, R).
remove_dups([H|T], [H|R]) :-
    remove_dups(T, R).

% Zip two lists
zip([], [], []).
zip([H1|T1], [H2|T2], [[H1,H2]|R]) :- zip(T1, T2, R).

% Example queries:
% ?- reverse([1, 2, 3, 4], R).
% ?- sum([1, 2, 3, 4, 5], S).
% ?- max_list([3, 1, 4, 1, 5, 9, 2, 6], M).
