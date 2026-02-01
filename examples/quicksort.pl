% Quicksort Algorithm
% A classic divide-and-conquer sorting algorithm.

quicksort([], []).
quicksort([Pivot|Rest], Sorted) :-
    partition(Pivot, Rest, Less, Greater),
    quicksort(Less, SortedLess),
    quicksort(Greater, SortedGreater),
    append(SortedLess, [Pivot|SortedGreater], Sorted).

% Partition a list into elements less than and greater than Pivot
partition(_, [], [], []).
partition(Pivot, [H|T], [H|Less], Greater) :-
    H =< Pivot, !,
    partition(Pivot, T, Less, Greater).
partition(Pivot, [H|T], Less, [H|Greater]) :-
    H > Pivot,
    partition(Pivot, T, Less, Greater).

% Append two lists
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Example:
% ?- quicksort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
