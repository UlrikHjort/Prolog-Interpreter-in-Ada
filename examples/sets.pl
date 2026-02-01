% Set Operations
% Implementing mathematical set operations on lists.

% Check membership
member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T).

% Check if list is a set (no duplicates)
is_set([]).
is_set([H|T]) :- \+ member(H, T), is_set(T).

% Convert list to set (remove duplicates)
list_to_set([], []).
list_to_set([H|T], S) :-
    member(H, T), !,
    list_to_set(T, S).
list_to_set([H|T], [H|S]) :-
    list_to_set(T, S).

% Union of two sets
union([], S, S).
union([H|T], S, U) :-
    member(H, S), !,
    union(T, S, U).
union([H|T], S, [H|U]) :-
    union(T, S, U).

% Intersection of two sets
intersection([], _, []).
intersection([H|T], S, [H|I]) :-
    member(H, S), !,
    intersection(T, S, I).
intersection([_|T], S, I) :-
    intersection(T, S, I).

% Difference: elements in S1 but not in S2
difference([], _, []).
difference([H|T], S, D) :-
    member(H, S), !,
    difference(T, S, D).
difference([H|T], S, [H|D]) :-
    difference(T, S, D).

% Symmetric difference: elements in S1 or S2 but not both
symmetric_diff(S1, S2, SD) :-
    difference(S1, S2, D1),
    difference(S2, S1, D2),
    union(D1, D2, SD).

% Subset check: is S1 a subset of S2?
subset([], _).
subset([H|T], S) :-
    member(H, S),
    subset(T, S).

% Proper subset: subset but not equal
proper_subset(S1, S2) :-
    subset(S1, S2),
    \+ subset(S2, S1).

% Set equality
set_equal(S1, S2) :-
    subset(S1, S2),
    subset(S2, S1).

% Power set (all subsets)
power_set([], [[]]).
power_set([H|T], PS) :-
    power_set(T, PS1),
    add_to_each(H, PS1, PS2),
    append(PS1, PS2, PS).

add_to_each(_, [], []).
add_to_each(X, [S|Rest], [[X|S]|Result]) :-
    add_to_each(X, Rest, Result).

% Cartesian product
cartesian([], _, []).
cartesian([H|T], S, P) :-
    pair_with(H, S, Pairs),
    cartesian(T, S, Rest),
    append(Pairs, Rest, P).

pair_with(_, [], []).
pair_with(X, [Y|T], [[X,Y]|Rest]) :-
    pair_with(X, T, Rest).

% Append
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Example queries:
% ?- union([1, 2, 3], [2, 3, 4], U).
% ?- intersection([1, 2, 3], [2, 3, 4], I).
% ?- difference([1, 2, 3], [2, 3, 4], D).
% ?- power_set([1, 2], PS).
% ?- subset([1, 2], [1, 2, 3]).
