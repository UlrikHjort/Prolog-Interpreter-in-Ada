% Mergesort Algorithm
% Another classic divide-and-conquer sorting algorithm.

mergesort([], []) :- !.
mergesort([X], [X]) :- !.
mergesort(List, Sorted) :-
    split(List, Left, Right),
    mergesort(Left, SortedLeft),
    mergesort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

% Split a list into two halves
split([], [], []).
split([X], [X], []).
split([X, Y|Rest], [X|Left], [Y|Right]) :-
    split(Rest, Left, Right).

% Merge two sorted lists
merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H1|T1], [H2|T2], [H1|Merged]) :-
    H1 =< H2, !,
    merge(T1, [H2|T2], Merged).
merge([H1|T1], [H2|T2], [H2|Merged]) :-
    H2 < H1,
    merge([H1|T1], T2, Merged).

% Example:
% ?- mergesort([5, 2, 8, 1, 9, 3], Sorted).
